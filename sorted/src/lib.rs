use proc_macro2::Span;
use quote::ToTokens;
use std::{fmt::Display, rc::Rc};
use syn::{
    parse_macro_input,
    spanned::Spanned,
    visit_mut::{self, VisitMut},
    Error, Result,
};

#[proc_macro_attribute]
pub fn sorted(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item = parse_macro_input!(input);
    let result = check_item(&item);
    let mut output = item.into_token_stream();
    if let Err(err) = result {
        output.extend(Error::into_compile_error(err))
    }
    output.into()
}

#[proc_macro_attribute]
pub fn check(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut item_fn = parse_macro_input!(input);
    let mut visitor = ExprMatchVisitor::new();
    visitor.visit_item_fn_mut(&mut item_fn);
    let mut output = item_fn.into_token_stream();
    if let Err(err) = visitor.result {
        output.extend(Error::into_compile_error(err))
    }
    output.into()
}

fn check_item(item: &syn::Item) -> Result<()> {
    if let syn::Item::Enum(ref enum_item) = item {
        validate_variants(&wildcard_ident(), enum_item)?
    } else {
        return err(Span::call_site(), "expected enum or match expression");
    };
    Ok(())
}

fn validate_variants<V>(wildcard_ident: &syn::Ident, item: V) -> Result<()>
where
    V: GetVariants,
{
    let variants: Vec<_> = item.get_variants(wildcard_ident)?;
    let sorted_variants = {
        let mut vs = variants.clone();
        vs.sort();
        vs
    };
    let out_of_order = variants.iter().zip(&sorted_variants).find_map(
        |pair @ (unsorted, sorted)| (unsorted != sorted).then_some(pair),
    );
    if let Some((unsorted, sorted)) = out_of_order {
        return err(
            // Only `nightly` compiler produces full span for a multi-segment path
            sorted.get_span(),
            &format!("{sorted} should sort before {unsorted}"),
        );
    }
    Ok(())
}

struct ExprMatchVisitor {
    wildcard_ident: syn::Ident,
    result: Result<()>,
}

impl ExprMatchVisitor {
    fn new() -> Self {
        Self {
            wildcard_ident: wildcard_ident(),
            result: Ok(()),
        }
    }
}

impl VisitMut for ExprMatchVisitor {
    fn visit_expr_match_mut(&mut self, expr: &mut syn::ExprMatch) {
        let len = expr.attrs.len();
        expr.attrs.retain(|attr| !attr.path().is_ident("sorted"));
        if expr.attrs.len() < len {
            let res = validate_variants(&self.wildcard_ident, &*expr);
            if let Err(ref mut err) = self.result {
                err.extend(res.err())
            } else {
                self.result = res
            }
        }
        visit_mut::visit_expr_match_mut(self, expr)
    }
}

trait GetVariants {
    fn get_variants<'a>(
        &'a self,
        wildcard_ident: &'a syn::Ident,
    ) -> Result<Vec<Variant<'a>>>;
}

#[derive(Clone)]
struct Variant<'a> {
    kind: VariantKind<'a>,
    segments: Rc<[&'a syn::Ident]>,
}

impl<'a> Variant<'a> {
    fn from_ident(ident: &'a syn::Ident) -> Self {
        Self {
            kind: VariantKind::Ident(ident),
            segments: Rc::new([ident]),
        }
    }

    fn from_path(path: &'a syn::Path) -> Self {
        Self {
            kind: VariantKind::Path(path),
            segments: path.segments.iter().map(|seg| &seg.ident).collect(),
        }
    }

    fn get_span(&self) -> Span {
        match self.kind {
            VariantKind::Ident(ident) => ident.span(),
            VariantKind::Path(path) => path.span(),
            VariantKind::Wildcard(token) => token.span(),
        }
    }
}

#[derive(Clone, Copy)]
enum VariantKind<'a> {
    Ident(&'a syn::Ident),
    Path(&'a syn::Path),
    Wildcard(&'a syn::token::Underscore),
}

impl<'a> GetVariants for &'a syn::ItemEnum {
    fn get_variants(
        &self,
        _wildcard_ident: &syn::Ident,
    ) -> Result<Vec<Variant>> {
        Ok(self
            .variants
            .iter()
            .map(|v| Variant::from_ident(&v.ident))
            .collect())
    }
}

impl GetVariants for &syn::ExprMatch {
    fn get_variants<'a>(
        &'a self,
        wildcard_ident: &'a syn::Ident,
    ) -> Result<Vec<Variant>> {
        self.arms
            .iter()
            .map(|arm| match arm.pat {
                syn::Pat::Ident(ref pat) => Ok(Variant::from_ident(&pat.ident)),
                syn::Pat::Struct(ref pat) => Ok(Variant::from_path(&pat.path)),
                syn::Pat::TupleStruct(ref pat) => {
                    Ok(Variant::from_path(&pat.path))
                }
                syn::Pat::Wild(ref pat) => Ok(Variant {
                    kind: VariantKind::Wildcard(&pat.underscore_token),
                    segments: Rc::new([wildcard_ident]),
                }),
                ref pat => err(pat.span(), "unsupported by #[sorted]"),
            })
            .collect()
    }
}

fn err<T>(span: Span, message: &str) -> Result<T> {
    Err(Error::new(span, message))
}

// Dummy single char ident that must always be sorted to the last position
fn wildcard_ident() -> syn::Ident {
    syn::Ident::new("\u{323AD}", Span::call_site())
}

mod variant {
    use super::*;

    impl PartialEq for Variant<'_> {
        fn eq(&self, other: &Self) -> bool {
            self.segments == other.segments
        }
    }

    impl Eq for Variant<'_> {}

    impl PartialOrd for Variant<'_> {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.segments.cmp(&other.segments))
        }
    }

    impl Ord for Variant<'_> {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.segments.cmp(&other.segments)
        }
    }

    impl Display for Variant<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.kind {
                VariantKind::Ident(ident) => ident.fmt(f),
                VariantKind::Path(path) => path
                    .to_token_stream()
                    .to_string()
                    .replace(" :: ", "::")
                    .fmt(f),
                VariantKind::Wildcard(_) => "_".fmt(f),
            }
        }
    }
}
