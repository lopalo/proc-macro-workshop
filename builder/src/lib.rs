use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Error, Result,
};

#[proc_macro_derive(Builder, attributes(builder))]
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
    let input: syn::DeriveInput = syn::parse2(input)?;

    let ident = &input.ident;
    let vis = input.vis;
    let builder_ident = format_ident!("{}Builder", input.ident);

    let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named: fields, .. }),
        ..
    }) = input.data else {
        return err(
            ident.span(),
            "Only a struct with named fields is supported"
        )};

    let field_name: Vec<_> = fields.iter().map(|f| f.ident.clone()).collect();

    let mut builder_field = vec![];
    let mut builder_setter = vec![];
    let mut build_expr = vec![];
    let mut error = Ok::<_, Error>(());
    let mut add_error = |field_error| match &mut error {
        Ok(()) => {
            error = Err(field_error);
        }
        Err(error) => {
            error.combine(field_error);
        }
    };
    for f in fields {
        let span = f.span();
        let f_ident = f.ident;
        let f_type = f.ty;
        let optional_type = optional_type(&f_type);
        let field_params = match field_params(&f.attrs) {
            Ok(params) => params,
            Err(field_err) => {
                add_error(field_err);
                continue;
            }
        };

        let b_field_ty = if field_params.each_setter_name.is_some() {
            f_type.to_token_stream()
        } else {
            let opt_ty = optional_type.unwrap_or(&f_type);
            quote! { ::std::option::Option<#opt_ty> }
        };
        builder_field.push(quote_spanned! {span=>
            #f_ident: #b_field_ty
        });

        if let Some(each_setter_name) = field_params.each_setter_name.as_ref() {
            let each_item_ty = match collection_item_type(&f_type) {
                Some(item_ty) => item_ty,
                None => {
                    add_error(Error::new(
                        span,
                        "The field must be a collection with a single generic type",
                    ));
                    continue;
                }
            };
            let each_setter_ident = syn::Ident::new(each_setter_name, span);
            builder_setter.push(quote_spanned! {span=>
                #vis fn #each_setter_ident(&mut self, value: #each_item_ty) -> &mut Self {
                    ::std::iter::Extend::extend(&mut self.#f_ident, [value]);
                    self
                }
            });

            if f_ident.as_ref().unwrap() != each_setter_name {
                builder_setter.push(quote_spanned! {span=>
                    #vis fn #f_ident(&mut self, value: #f_type) -> &mut Self {
                        self.#f_ident = value;
                        self
                    }
                });
            }
        } else {
            let b_setter_ty = match optional_type {
                Some(ty) => ty,
                None => &f_type,
            };
            builder_setter.push(quote_spanned! {span=>
                #vis fn #f_ident(&mut self, value: #b_setter_ty) -> &mut Self {
                    self.#f_ident = ::std::option::Option::Some(value);
                    self
                }
            });
        }

        build_expr.push(
            if optional_type.is_none()
                && field_params.each_setter_name.is_none()
            {
                quote! {
                    #f_ident: #f_ident.ok_or_else(||
                        format!("Field `{}` is not set", stringify!(#f_ident))
                    )?
                }
            } else {
                f_ident.to_token_stream()
            },
        );
    }

    error?;

    let output = quote! {
        impl #ident {
            #vis fn builder() -> #builder_ident {
                #builder_ident {
                    #(#field_name: ::std::default::Default::default()),*
                }
            }
        }

        #vis struct #builder_ident {
            #(#builder_field),*
        }

        impl #builder_ident {
            #(#builder_setter)*

            #vis fn build(&mut self) -> ::std::result::Result<#ident, ::std::boxed::Box<dyn ::std::error::Error>> {
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

fn optional_type(ty: &syn::Type) -> Option<&syn::Type> {
    let syn::Type::Path(syn::TypePath { path, .. }) = ty else { return None };
    let segment = path.segments.first()?;
    if segment.ident != "Option" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(ref option_args) = segment.arguments else {
        return None
    };
    let syn::GenericArgument::Type(optional_type) = option_args.args.first()? else {
        return None
    };
    Some(optional_type)
}

fn collection_item_type(ty: &syn::Type) -> Option<&syn::Type> {
    let syn::Type::Path(syn::TypePath { path, .. }) = ty else { return None };
    let segment = path.segments.first()?;
    let syn::PathArguments::AngleBracketed(ref generic_args) = segment.arguments else {
        return None
    };
    let mut generic_args = generic_args.args.iter();
    let syn::GenericArgument::Type(item_type) = generic_args.next()? else {
        return None
    };
    if let Some(_) = generic_args.next() {
        return None;
    }
    Some(item_type)
}

#[derive(Default)]
struct FieldParams {
    each_setter_name: Option<String>,
}

struct FieldParam {
    span: Span,
    name: String,
    value: String,
}

impl Parse for FieldParam {
    fn parse(input: ParseStream) -> Result<Self> {
        let name_ident: syn::Ident = input.parse()?;
        let span = name_ident.span();
        let name = name_ident.to_string();
        input.parse::<syn::token::Eq>()?;
        let value = input.parse::<syn::LitStr>()?.value();
        Ok(Self { span, name, value })
    }
}

fn field_params(attrs: &[syn::Attribute]) -> Result<FieldParams> {
    let mut params = FieldParams::default();
    for attr in attrs {
        if !attr.path().is_ident("builder") {
            continue;
        }
        let param: FieldParam = attr.parse_args()?;
        let error = |message| err(param.span, message);
        match param.name.as_str() {
            "each" => {
                if params.each_setter_name.is_some() {
                    return error("Duplicated `each` parameter");
                }
                params.each_setter_name = Some(param.value)
            }
            param_name => {
                return error(&format!("Unrecognized `{param_name}` parameter"))
            }
        }
    }
    Ok(params)
}

#[cfg(test)]
mod test {
    use super::*;

    #[track_caller]
    fn assert_tokens_eq(actual: TokenStream, expected: TokenStream) {
        let actual: syn::File = syn::parse2(actual).unwrap();
        let expected: syn::File = syn::parse2(expected).unwrap();
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
                #[builder(each = "arg")]
                args: Vec<String>,
                #[builder(each = "env")]
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
                            executable: ::std::default::Default::default(),
                            args: ::std::default::Default::default(),
                            env: ::std::default::Default::default(),
                            current_dir: ::std::default::Default::default(),
                        }
                    }
                }

                pub struct CommandBuilder {
                    executable: ::std::option::Option<String>,
                    args: Vec<String>,
                    env: Vec<String>,
                    current_dir: ::std::option::Option<String>,
                }

                impl CommandBuilder {
                    pub fn executable(&mut self, value: String) -> &mut Self {
                        self.executable = ::std::option::Option::Some(value);
                        self
                    }
                    pub fn arg(&mut self, value: String) -> &mut Self {
                        ::std::iter::Extend::extend(&mut self.args, [value]);
                        self
                    }
                    pub fn args(&mut self, value: Vec<String>) -> &mut Self {
                        self.args = value;
                        self
                    }
                    pub fn env(&mut self, value: String) -> &mut Self {
                        ::std::iter::Extend::extend(&mut self.env, [value]);
                        self
                    }
                    pub fn current_dir(&mut self, value: String) -> &mut Self {
                        self.current_dir = ::std::option::Option::Some(value);
                        self
                    }
                    pub fn build(&mut self) -> ::std::result::Result<Command, ::std::boxed::Box<dyn ::std::error::Error>> {
                        let Self {
                            executable,
                            args,
                            env,
                            current_dir,
                        } = ::std::mem::replace(self, Command::builder());
                        ::std::result::Result::Ok(Command {
                            executable: executable
                                .ok_or_else(|| format!("Field `{}` is not set", stringify!(executable)))?,
                            args,
                            env,
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
