use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    parse2, spanned::Spanned, Data, DataStruct, DeriveInput, Error, Fields,
    FieldsNamed, GenericArgument, PathArguments, Result, Type, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    expand_with_error(input.into()).into()
}

fn expand_with_error(input: TokenStream) -> TokenStream {
    expand(input).unwrap_or_else(Error::into_compile_error)
}

fn err<T>(span: Span, message: &str) -> Result<T> {
    Err(Error::new(span, message))
}

fn expand(input: TokenStream) -> Result<TokenStream> {
    let input: DeriveInput = parse2(input)?;

    let ident = &input.ident;
    let vis = input.vis;
    let builder_ident = format_ident!("{}Builder", input.ident);

    let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { named: fields, .. }),
        ..
    }) = input.data else {
        return err(
            ident.span(),
            "Only a struct with named fields is supported"
        )};

    let field_name: Vec<_> = fields.iter().map(|f| f.ident.clone()).collect();

    let (builder_field, (builder_setter, build_expr)) = fields
        .into_iter()
        .map(|f| {
            let span = f.span();
            let f_name = f.ident;
            let f_type = f.ty;
            let optional_type = optional_type(&f_type);

            let b_field_ty = optional_type.unwrap_or(&f_type);
            let b_field = quote_spanned! {span=>
                #f_name: Option<#b_field_ty>
            };

            let b_setter_ty = match optional_type {
                Some(ty) => ty,
                None => &f_type,
            };
            let b_setter = quote_spanned! {span=>
                #vis fn #f_name(&mut self, value: #b_setter_ty) -> &mut Self {
                    self.#f_name = Some(value);
                    self
                }
            };

            let b_expr = if optional_type.is_none() {
                quote! {
                    #f_name: #f_name.ok_or_else(||
                        format!("Field `{}` is not set", stringify!(#f_name))
                    )?
                }
            } else {
                f_name.to_token_stream()
            };

            (b_field, (b_setter, b_expr))
        })
        .unzip::<_, _, Vec<_>, (Vec<_>, Vec<_>)>();

    let output = quote! {
        impl #ident {
            #vis fn builder() -> #builder_ident {
                #builder_ident {
                    #(#field_name: None),*
                }
            }
        }

        #vis struct #builder_ident {
            #(#builder_field),*
        }

        impl #builder_ident {
            #(#builder_setter)*

            #vis fn build(&mut self) -> ::std::result::Result<#ident, Box<dyn ::std::error::Error>> {
                let Self {
                    #(#field_name),*
                } = ::std::mem::replace(self, #ident::builder());
                ::std::result::Result::Ok(#ident {
                    #(#build_expr),*
                })
            }
        }
    };

    Ok(output)
}

fn optional_type(ty: &Type) -> Option<&Type> {
    let Type::Path(TypePath { path, .. }) = ty else { return None };
    let segment = path.segments.first()?;
    if segment.ident.to_string() != "Option" {
        return None;
    }
    let PathArguments::AngleBracketed(ref option_args) = segment.arguments else { return None };
    let Some(GenericArgument::Type(optional_type)) = option_args.args.first() else { return None };
    Some(optional_type)
}

#[cfg(test)]
mod test {
    use super::*;
    use syn::File;

    #[track_caller]
    fn assert_tokens_eq(actual: TokenStream, expected: TokenStream) {
        let actual: File = parse2(actual).unwrap();
        let expected: File = parse2(expected).unwrap();
        pretty_assertions::assert_eq!(
            prettyplease::unparse(&actual),
            prettyplease::unparse(&expected),
        )
    }

    fn compile_err(message: &str) -> TokenStream {
        quote! {
            ::core::compile_error! {
               #message
            }
        }
    }

    #[test]
    fn simple_struct() {
        let struct_item = quote! {
            pub struct Command {
                executable: String,
                args: Vec<String>,
                env: Vec<String>,
                current_dir: Option<String>,
            }
        };
        assert_tokens_eq(
            expand_with_error(struct_item),
            quote! {
                impl Command {
                    pub fn builder() -> CommandBuilder {
                        CommandBuilder {
                            executable: None,
                            args: None,
                            env: None,
                            current_dir: None,
                        }
                    }
                }

                pub struct CommandBuilder {
                    executable: Option<String>,
                    args: Option<Vec<String>>,
                    env: Option<Vec<String>>,
                    current_dir: Option<String>,
                }

                impl CommandBuilder {
                    pub fn executable(&mut self, value: String) -> &mut Self {
                        self.executable = Some(value);
                        self
                    }
                    pub fn args(&mut self, value: Vec<String>) -> &mut Self {
                        self.args = Some(value);
                        self
                    }
                    pub fn env(&mut self, value: Vec<String>) -> &mut Self {
                        self.env = Some(value);
                        self
                    }
                    pub fn current_dir(&mut self, value: String) -> &mut Self {
                        self.current_dir = Some(value);
                        self
                    }
                    pub fn build(&mut self) -> ::std::result::Result<Command, Box<dyn ::std::error::Error>> {
                        let Self {
                            executable,
                            args,
                            env,
                            current_dir,
                        } = ::std::mem::replace(self, Command::builder());
                        ::std::result::Result::Ok(Command {
                            executable: executable
                                .ok_or_else(|| format!("Field `{}` is not set", stringify!(executable)))?,
                            args: args.ok_or_else(|| format!("Field `{}` is not set", stringify!(args)))?,
                            env: env.ok_or_else(|| format!("Field `{}` is not set", stringify!(env)))?,
                            current_dir,
                        })

                   }
                }
            },
        )
    }

    #[test]
    fn named_tuple() {
        let struct_item = quote! {
            pub struct FooCommand(String, Vec<String>);
        };
        assert_tokens_eq(
            expand_with_error(struct_item),
            compile_err("Only a struct with named fields is supported"),
        )
    }
}
