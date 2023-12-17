use darling::{ast, Error, FromDeriveInput, FromField, Result};
use proc_macro2::TokenStream;
use quote::quote;

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    expand(input.into())
        .unwrap_or_else(Error::write_errors)
        .into()
}

fn expand(input: TokenStream) -> Result<TokenStream> {
    let input: syn::DeriveInput = syn::parse2(input)?;
    let dbg_struct = DebugStruct::from_derive_input(&input)?;

    let struct_ident = dbg_struct.ident;
    let dbg_fields = dbg_struct.data.take_struct().unwrap();
    let field_fmt = dbg_fields.into_iter().map(|f| {
        let f_ident = f.ident;
        quote! {
            .field(stringify!(#f_ident), &self.#f_ident)
        }
    });

    let output = quote! {
        impl ::std::fmt::Debug for #struct_ident {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                fmt.debug_struct(stringify!(#struct_ident))
                    #(#field_fmt)*
                    .finish()
            }
        }
    };

    Ok(output)
}

#[derive(FromDeriveInput)]
#[darling(supports(struct_named))]
struct DebugStruct {
    ident: syn::Ident,
    data: ast::Data<(), DebugField>,
}

#[derive(FromField)]
struct DebugField {
    ident: Option<syn::Ident>,
}
