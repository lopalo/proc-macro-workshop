use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{
    parse2, Data, DataStruct, DeriveInput, Error, Fields, FieldsNamed, Result,
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

    let builder_field = fields.into_iter().map(|f| {
        let fname = f.ident;
        let ftype = f.ty;
        quote! {
            #fname: Option<#ftype>
        }
    });

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
    };

    Ok(output)
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
                current_dir: String,
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
