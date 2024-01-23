use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Error, Result,
};

#[proc_macro_attribute]
pub fn bitfield(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    expand_bitfield(input.into())
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn expand_bitfield(input: TokenStream) -> Result<TokenStream> {
    let syn::ItemStruct {
        vis, ident, fields, ..
    } = syn::parse2(input)?;
    let syn::Fields::Named(syn::FieldsNamed { named: fields, .. }) = fields else {
        return err(fields.span(), "Only named fields are supported")
    };

    let mut total_bits = quote! { 0_usize };
    for field in fields {
        let ty = field.ty;
        let bits_expr = quote! {<#ty as ::bitfield::Specifier>::BITS};
        total_bits.extend(quote! {
            + #bits_expr
        })
    }
    let struct_bytes_ident = format_ident!("{}_BYTES", ident);

    let output = quote! {

        #[allow(non_upper_case_globals)]
        const #struct_bytes_ident: usize = (#total_bits).div_ceil(8);

        #vis struct #ident {data: [u8; #struct_bytes_ident]}

        impl #ident {
            #vis fn new() -> Self {
                Self {data: [0; #struct_bytes_ident]}
            }
        }
    };

    Ok(output)
}

fn err<T>(span: Span, message: &str) -> Result<T> {
    Err(Error::new(span, message))
}

/// Private helper macro
#[proc_macro]
pub fn define_specifiers(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let SpecifiersRange { prefix, start, end } = syn::parse(input).unwrap();

    let mut output = TokenStream::new();
    for n in start..=end {
        let ident = format_ident!("{}{n}", prefix);
        output.extend(quote! {
            pub enum #ident {}

            impl Specifier for #ident {
                const BITS: usize = #n;
            }
        });
    }
    output.into()
}

struct SpecifiersRange {
    prefix: syn::Ident,
    start: usize,
    end: usize,
}

impl Parse for SpecifiersRange {
    fn parse(input: ParseStream) -> Result<Self> {
        let prefix = input.parse()?;
        let start = syn::LitInt::parse(input)?.base10_parse()?;
        let end = syn::LitInt::parse(input)?.base10_parse()?;
        Ok(Self { prefix, start, end })
    }
}
