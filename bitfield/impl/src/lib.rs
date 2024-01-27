use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
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
    let syn::Fields::Named(syn::FieldsNamed { named: fields, .. }) = fields
    else {
        return err(fields.span(), "Only named fields are supported");
    };

    let struct_bytes_len_ident = format_ident!("{}_BYTES", ident);

    let mut bits_offset = quote! { 0_usize };
    let mut accessors = vec![];
    for field in fields {
        let span = field.span();
        let ident = field.ident.unwrap();
        let ty = field.ty;
        let spec = quote! {<#ty as ::bitfield::Specifier>};
        let bits_len = quote! {#spec::BITS};

        let setter_ident = format_ident!("set_{}", ident);
        let getter_ident = format_ident!("get_{}", ident);
        accessors.push(quote_spanned! {span=>
            pub fn #setter_ident(&mut self, item: #spec::Item) {
                const _: () = assert!(
                    #bits_len.div_ceil(8) <= #spec::ZERO_ITEM_BYTES.len(),
                    "The `Specifier::ItemBytes` array cannot accomodate `Specifier::BITS`"
                );
                ::bitfield::write_bits(
                    &mut self.data,
                    #bits_offset,
                    #spec::item_to_bytes(item).as_mut_slice(),
                    #bits_len
                )
            }

            pub fn #getter_ident(&self) -> #spec::Item {
                let mut bytes = #spec::ZERO_ITEM_BYTES;
                ::bitfield::read_bits(
                    &self.data,
                    #bits_offset,
                    bytes.as_mut_slice(),
                    #bits_len
                );
                #spec::item_from_bytes(bytes)
            }
        });
        bits_offset = quote! {#bits_offset + #bits_len};
    }

    let output = quote! {

        #[allow(non_upper_case_globals)]
        const #struct_bytes_len_ident: usize = (#bits_offset).div_ceil(8);

        #vis struct #ident {data: [u8; #struct_bytes_len_ident]}

        impl #ident {
            pub fn new() -> Self {
                Self {data: [0; #struct_bytes_len_ident]}
            }

            #(#accessors)*
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
    let SpecifiersRange {
        prefix,
        start,
        end,
        item_ty,
    } = syn::parse(input).unwrap();

    let mut output = TokenStream::new();
    for bits_len in start..=end {
        let ident = format_ident!("{}{bits_len}", prefix);
        let bytes_len = quote! {#item_ty::BITS.div_ceil(8) as usize};
        output.extend(quote! {
            pub enum #ident {}

            impl Specifier for #ident {
                const BITS: usize = #bits_len;
                type Item = #item_ty;
                type ItemBytes = [u8; #bytes_len];
                const ZERO_ITEM_BYTES: Self::ItemBytes = [0; #bytes_len];

                fn item_to_bytes(item: Self::Item) -> Self::ItemBytes {
                    item.to_le_bytes()
                }

                fn item_from_bytes(bytes: Self::ItemBytes) -> Self::Item {
                    Self::Item::from_le_bytes(bytes)
                }
            }
        });
    }
    output.into()
}

struct SpecifiersRange {
    prefix: syn::Ident,
    start: usize,
    end: usize,
    item_ty: syn::Ident,
}

impl Parse for SpecifiersRange {
    fn parse(input: ParseStream) -> Result<Self> {
        let prefix = input.parse()?;
        let start = syn::LitInt::parse(input)?.base10_parse()?;
        let end = syn::LitInt::parse(input)?.base10_parse()?;
        let item_ty = input.parse()?;
        Ok(Self {
            prefix,
            start,
            end,
            item_ty,
        })
    }
}
