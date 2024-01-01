use proc_macro2::Span;
use quote::ToTokens;
use syn::{
    parse_macro_input,
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
    let mut variants: Vec<_> = item.get_variants();
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

trait GetVariants {
    fn get_variants(&self) -> Vec<&syn::Ident>;
}

impl GetVariants for &syn::ItemEnum {
    fn get_variants(&self) -> Vec<&syn::Ident> {
        self.variants.iter().map(|v| &v.ident).collect()
    }
}

impl GetVariants for &syn::ExprMatch {
    fn get_variants(&self) -> Vec<&syn::Ident> {
        println!("{:?}", self.arms);
        self.arms
            .iter()
            .filter_map(|arm| match arm.pat {
                syn::Pat::Struct(ref pat) => {
                    pat.path.segments.last().map(|s| &s.ident)
                }
                syn::Pat::TupleStruct(ref pat) => {
                    pat.path.segments.last().map(|s| &s.ident)
                }
                _ => None,
            })
            .collect()
    }
}

fn err<T>(span: Span, message: &str) -> Result<T> {
    Err(Error::new(span, message))
}
