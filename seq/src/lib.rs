use proc_macro2::{Delimiter, Group, Literal, Span, TokenStream, TokenTree};
use quote::format_ident;
use std::ops::Range;
use syn::{
    parse::{Parse, ParseStream},
    Error, Result,
};

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    expand(input.into())
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn expand(input: TokenStream) -> Result<TokenStream> {
    let Seq { header, body } = syn::parse2(input)?;
    let body = body.stream();
    let mut output = TokenStream::new();
    let mut repeat_body = true;
    for n in header.range.clone() {
        output.extend(replace_in(&header, &mut repeat_body, n, body.clone())?);
        if !repeat_body {
            break;
        }
    }
    Ok(output)
}

fn replace_in(
    header: &SeqHeader,
    repeat_body: &mut bool,
    var_value: u16,
    input: TokenStream,
) -> Result<TokenStream> {
    let mut input = input.into_iter();
    let mut output = Vec::new();
    while let Some(token) = input.next() {
        match token {
            TokenTree::Ident(ref ident) if ident == &header.var_name => {
                let mut replacement = Literal::u16_unsuffixed(var_value);
                replacement.set_span(ident.span());
                output.push(replacement.into())
            }
            TokenTree::Group(group) => {
                let span = group.span();
                let stream =
                    replace_in(header, repeat_body, var_value, group.stream())?;
                let mut group = Group::new(group.delimiter(), stream);
                group.set_span(span);
                output.push(group.into())
            }
            TokenTree::Punct(punct) if punct.as_char() == '~' => {
                let prefix =
                    if let Some(TokenTree::Ident(prefix)) = output.pop() {
                        prefix
                    } else {
                        return err(
                            punct.span(),
                            "`~` must be prefixed with an identifier",
                        );
                    };
                if let Some(TokenTree::Ident(suffix)) = input.next() {
                    output.push(if suffix == &header.var_name {
                        format_ident!("{}{}", prefix, var_value).into()
                    } else {
                        format_ident!("{}{}", prefix, suffix).into()
                    })
                } else {
                    return err(
                            punct.span(),
                            "`~` must be followed by an identifier or the loop variable",
                        );
                }
            }
            TokenTree::Punct(prefix_punct) if prefix_punct.as_char() == '#' => {
                let (section_token, suffix_token) =
                    (input.next(), input.next());
                match (section_token.as_ref(), suffix_token.as_ref()) {
                    (
                        Some(TokenTree::Group(section_group)),
                        Some(TokenTree::Punct(suffix_punct)),
                    ) if section_group.delimiter()
                        == Delimiter::Parenthesis
                        && suffix_punct.as_char() == '*' =>
                    {
                        for n in header.range.clone() {
                            output.extend(replace_in(
                                &header,
                                repeat_body,
                                n,
                                section_group.stream(),
                            )?);
                        }
                        *repeat_body = false
                    }
                    _ => {
                        output.push(prefix_punct.into());
                        output.extend(section_token);
                        output.extend(suffix_token);
                    }
                }
            }
            TokenTree::Ident(_)
            | TokenTree::Punct(_)
            | TokenTree::Literal(_) => output.push(token.into()),
        }
    }
    Ok(output.into_iter().collect())
}

fn err<T>(span: Span, message: &str) -> Result<T> {
    Err(Error::new(span, message))
}

struct Seq {
    header: SeqHeader,
    body: Group,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            header: input.parse()?,
            body: input.parse()?,
        })
    }
}

struct SeqHeader {
    var_name: String,
    range: Range<u16>,
}

impl Parse for SeqHeader {
    fn parse(input: ParseStream) -> Result<Self> {
        let var_name = input.parse::<syn::Ident>()?.to_string();
        input.parse::<syn::Token![in]>()?;
        let start = input.parse::<syn::LitInt>()?.base10_parse()?;
        input.parse::<syn::Token![..]>()?;
        let end = input.parse::<syn::LitInt>()?.base10_parse()?;
        Ok(Self {
            var_name,
            range: Range { start, end },
        })
    }
}
