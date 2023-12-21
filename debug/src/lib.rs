use darling::{ast, Error, FromDeriveInput, FromField, Result};
use proc_macro2::TokenStream;
use quote::quote;
use std::collections::HashSet;
use syn::punctuated::Punctuated;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    expand(input.into())
        .unwrap_or_else(Error::write_errors)
        .into()
}

fn expand(input: TokenStream) -> Result<TokenStream> {
    let input: syn::DeriveInput = syn::parse2(input)?;
    let dbg_struct = DebugStruct::from_derive_input(&input)?;

    let struct_ident = dbg_struct.ident;
    let mut generics = dbg_struct.generics;
    let dbg_fields = dbg_struct
        .data
        .take_struct()
        .expect("BUG: `darling(supports(struct_named))` didn't work");

    let ty_param_idents: HashSet<_> = generics
        .type_params()
        .map(|ty_param| &ty_param.ident)
        .collect();

    let mut bounds = Vec::<syn::WherePredicate>::new();
    if let Some(bound) = dbg_struct.bound {
        bounds.extend(bound)
    } else {
        // Infer trait bounds from fields
        let mut debug_bounds = HashSet::new();
        for field in dbg_fields.iter() {
            collect_debug_bounds(
                &ty_param_idents,
                &mut debug_bounds,
                &field.ty,
            );
        }
        bounds.extend(
            debug_bounds
                .into_iter()
                .map(|ty| syn::parse_quote! {#ty: ::std::fmt::Debug}),
        )
    }

    generics.make_where_clause().predicates.extend(bounds);

    let field_fmt = dbg_fields.into_iter().map(|f| {
        let f_ident = f.ident;
        let format = f.format.as_deref().unwrap_or("{:?}");
        quote! {
            .field(stringify!(#f_ident), &format_args!(#format, &self.#f_ident))
        }
    });

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let output = quote! {
        impl #impl_generics ::std::fmt::Debug for #struct_ident #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                fmt.debug_struct(stringify!(#struct_ident))
                    #(#field_fmt)*
                    .finish()
            }
        }
    };

    Ok(output)
}

fn collect_debug_bounds(
    ty_param_idents: &HashSet<&syn::Ident>,
    debug_bounds: &mut HashSet<syn::Type>,
    ty: &syn::Type,
) {
    let syn::Type::Path(syn::TypePath { path, .. }) = &ty else { return };
    let (Some(first_segment), Some(last_segment)) = (
        path.segments.first(),
        path.segments.last()
    ) else { return };
    // PhantomData already implements `Debug`
    if last_segment.ident.to_string() == "PhantomData" {
        return;
    }
    if ty_param_idents.contains(&first_segment.ident) {
        if !debug_bounds.contains(&ty) {
            // `ty` might be a type parameter or an associated type
            debug_bounds.insert(ty.clone());
        }
        return;
    }
    let syn::PathArguments::AngleBracketed(ref ty_args) = last_segment.arguments else {
        return
    };
    for ty_arg in &ty_args.args {
        if let syn::GenericArgument::Type(ref ty) = ty_arg {
            collect_debug_bounds(ty_param_idents, debug_bounds, ty)
        }
    }
}

#[derive(FromDeriveInput)]
#[darling(supports(struct_named), attributes(debug))]
struct DebugStruct {
    ident: syn::Ident,
    generics: syn::Generics,
    data: ast::Data<(), DebugField>,
    bound: Option<Punctuated<syn::WherePredicate, syn::Token![,]>>,
}

#[derive(FromField)]
#[darling(attributes(debug))]
struct DebugField {
    ident: Option<syn::Ident>,
    ty: syn::Type,
    format: Option<String>,
}
