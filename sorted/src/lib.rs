use proc_macro2::Span;
use quote::ToTokens;
use std::{borrow::Cow, fmt::Display};
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
        validate_variants(enum_item)?
    } else {
        return err(Span::call_site(), "expected enum or match expression");
    };
    Ok(())
}

fn validate_variants<V>(item: V) -> Result<()>
where
    V: GetVariants,
{
    let mut variants: Vec<_> = item.get_variants()?;
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
            // Only `nightly` compiler produces full span for a multi-segment path
            out_of_order.get_span(),
            &format!("{out_of_order} should sort before {next}"),
        );
    }
    Ok(())
}

struct ExprMatchVisitor {
    result: Result<()>,
}

impl ExprMatchVisitor {
    fn new() -> Self {
        Self { result: Ok(()) }
    }
}

impl VisitMut for ExprMatchVisitor {
    fn visit_expr_match_mut(&mut self, expr: &mut syn::ExprMatch) {
        let len = expr.attrs.len();
        expr.attrs.retain(|attr| !attr.path().is_ident("sorted"));
        if expr.attrs.len() < len {
            let res = validate_variants(&*expr);
            if let Err(ref mut err) = self.result {
                err.extend(res.err())
            } else {
                self.result = res
            }
        }
        visit_mut::visit_expr_match_mut(self, expr)
    }
}

// Define custom trait since `syn::spanned:Spanned` is a sealed
trait GetSpan {
    fn get_span(&self) -> Span;
}

trait GetVariants {
    type Variant: Copy + Ord + Display + GetSpan;

    fn get_variants(&self) -> Result<Vec<Self::Variant>>;
}

#[derive(PartialEq, Eq, Clone, Copy)]
struct Variant<T>(T);

impl<'a> GetVariants for &'a syn::ItemEnum {
    type Variant = Variant<&'a syn::Ident>;

    fn get_variants(&self) -> Result<Vec<Self::Variant>> {
        Ok(self.variants.iter().map(|v| Variant(&v.ident)).collect())
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum MatchVariant<'a> {
    Ident(&'a syn::Ident),
    Path(&'a syn::Path),
    Wildcard(&'a syn::token::Underscore),
}

impl<'a> MatchVariant<'a> {
    fn segments(&self) -> Box<dyn Iterator<Item = Cow<syn::Ident>> + '_> {
        match self {
            Self::Ident(ident) => Box::new([Cow::Borrowed(*ident)].into_iter()),
            Self::Path(path) => Box::new(
                path.segments.iter().map(|seg| Cow::Borrowed(&seg.ident)),
            ),
            Self::Wildcard(token) => Box::new(
                [Cow::Owned(syn::Ident::new("\u{323AD}", token.span()))].into_iter(),
            ),
        }
    }
}

impl<'a> GetVariants for &'a syn::ExprMatch {
    type Variant = Variant<MatchVariant<'a>>;

    fn get_variants(&self) -> Result<Vec<Self::Variant>> {
        self.arms
            .iter()
            .map(|arm| match arm.pat {
                syn::Pat::Ident(ref pat) => Ok(MatchVariant::Ident(&pat.ident)),
                syn::Pat::Struct(ref pat) => Ok(MatchVariant::Path(&pat.path)),
                syn::Pat::TupleStruct(ref pat) => {
                    Ok(MatchVariant::Path(&pat.path))
                }
                syn::Pat::Wild(ref pat) => {
                    Ok(MatchVariant::Wildcard(&pat.underscore_token))
                }
                ref pat => err(pat.span(), "unsupported by #[sorted]"),
            })
            .map(|r| r.map(Variant))
            .collect()
    }
}

fn err<T>(span: Span, message: &str) -> Result<T> {
    Err(Error::new(span, message))
}

mod variant {
    use super::*;

    impl<T> PartialOrd for Variant<T>
    where
        Variant<T>: Ord,
    {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    impl GetSpan for Variant<&syn::Ident> {
        fn get_span(&self) -> Span {
            self.0.span()
        }
    }

    impl Ord for Variant<&syn::Ident> {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.0.cmp(&other.0)
        }
    }

    impl Display for Variant<&syn::Ident> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.fmt(f)
        }
    }

    impl GetSpan for Variant<MatchVariant<'_>> {
        fn get_span(&self) -> Span {
            match self.0 {
                MatchVariant::Ident(ident) => ident.span(),
                MatchVariant::Path(path) => path.span(),
                MatchVariant::Wildcard(token) => token.span(),
            }
        }
    }

    impl Ord for Variant<MatchVariant<'_>> {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.0.segments().cmp(other.0.segments())
        }
    }

    impl Display for Variant<MatchVariant<'_>> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.0 {
                MatchVariant::Ident(ident) => ident.fmt(f),
                MatchVariant::Path(path) => path
                    .to_token_stream()
                    .to_string()
                    .replace(" :: ", "::")
                    .fmt(f),
                MatchVariant::Wildcard(_) => "_".fmt(f),
            }
        }
    }
}
