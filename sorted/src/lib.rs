use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::{Error, Result};

#[proc_macro_attribute]
pub fn sorted(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    parse_and_validate(args.into(), input.into())
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn parse_and_validate(
    _args: TokenStream,
    input: TokenStream,
) -> Result<TokenStream> {
    let item: syn::Item = syn::parse2(input)?;
    if let syn::Item::Enum(ref enum_item) = item {
        validate_enum(enum_item)?
    } else {
        return err(Span::call_site(), "expected enum or match expression");
    };
    Ok(item.into_token_stream())
}

fn validate_enum(item: &syn::ItemEnum) -> Result<()> {
    let mut variants: Vec<_> = item.variants.iter().map(|v| &v.ident).collect();
    if variants.is_empty() {
        return Ok(());
    }
    let out_of_order = variants
        .windows(2)
        .find_map(|pair| (pair[0] > pair[1]).then_some(pair[1]));
    if let Some(out_of_order) = out_of_order {
        variants.sort();
        let idx = variants.binary_search(&out_of_order).unwrap();
        let next = variants
            .get(idx + 1)
            .unwrap_or_else(|| variants.last().unwrap());
        return err(
            out_of_order.span(),
            &format!("{out_of_order} should sort before {next}"),
        );
    }
    Ok(())
}

fn err<T>(span: Span, message: &str) -> Result<T> {
    Err(Error::new(span, message))
}
