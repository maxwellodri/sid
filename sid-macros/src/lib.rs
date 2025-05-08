use proc_macro::TokenStream;
use proc_macro2::{Span as Span2, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::{parse_macro_input, visit::Visit, Expr, Ident, Item, ItemImpl, ItemMod, LitStr};

/// Generates an array of SIDs based on a list of string literals
///
/// # Examples
///
/// ```
/// use sid::sid_array;
///
/// let state_ids = sid_array!["Idle", "Walking", "Running", "Jumping"];
/// for state_id in state_ids {
///    println!("State: {}", state_id);
/// }
/// ```
#[proc_macro]
pub fn sid_array(tokens: TokenStream) -> TokenStream {
    let expr = parse_macro_input!(tokens as Expr);

    match expr {
        Expr::Array(array) => {
            let elements = array.elems.iter().map(|e| match e {
                Expr::Lit(lit) => {
                    if let syn::Lit::Str(str_lit) = &lit.lit {
                        let string_value = str_lit.value();
                        quote! { ::sid::SID::from(#string_value) }
                    } else {
                        syn::Error::new_spanned(e, "sid_array expects string literals")
                            .to_compile_error()
                    }
                }
                _ => syn::Error::new_spanned(e, "sid_array expects an array of string literals")
                    .to_compile_error(),
            });
            quote! {
                [#(#elements),*]
            }
            .into()
        }
        _ => syn::Error::new_spanned(
            expr.to_token_stream(),
            "sid_array expects an array of string literals",
        )
        .to_compile_error()
        .into(),
    }
}

#[proc_macro_attribute]
pub fn sid_iter(attrs: TokenStream, input: TokenStream) -> TokenStream {
    sid_iter_internal(attrs, input).into()
}

struct LitStrCollector(Vec<String>);

impl<'ast> Visit<'ast> for LitStrCollector {
    fn visit_lit_str(&mut self, lit: &'ast LitStr) {
        self.0.push(lit.value());
    }
}

pub(crate) fn sid_iter_internal(attrs: TokenStream, input: TokenStream) -> TokenStream2 {
    let input_clone = input.clone();

    if let Ok(impl_block) = syn::parse::<ItemImpl>(input_clone) {
        parse_item_impl(impl_block, attrs)
    } else if let Ok(module) = syn::parse::<ItemMod>(input) {
        parse_mod(module, attrs)
    } else {
        quote! {
            compile_error!("sid_iter needs to annotate either an impl block or a module")
        }
    }
}

fn parse_item_impl(mut item: ItemImpl, attrs: TokenStream) -> TokenStream2 {
    let mut collector = LitStrCollector(Vec::new());
    collector.visit_item_impl(&item);
    item.items.push(syn::ImplItem::Verbatim(sid_register_tokens(
        collector.0,
        attrs,
    )));
    item.to_token_stream()
}

fn parse_mod(mut item: ItemMod, attrs: TokenStream) -> TokenStream2 {
    let mut collector = LitStrCollector(Vec::new());
    collector.visit_item_mod(&item);
    if let Some((_, ref mut content)) = item.content {
        let func = sid_register_tokens(collector.0, attrs);
        content.push(Item::Verbatim(func));
    } else {
        return quote! { compile_error!("sid_iter needs to annotate mod items that directly contain other items") };
    }
    item.to_token_stream()
}

fn sid_register_tokens(strings: Vec<String>, attrs: TokenStream) -> TokenStream2 {
    let function_name = if attrs.is_empty() {
        Ident::new("sid_register", Span2::call_site())
    } else if let Ok(function_name) = syn::parse_str::<Ident>(&attrs.to_string()) {
        function_name
    } else {
        return quote! { compile_error!("#attrs cannot be parsed as a valid Rust identifier") };
    };

    quote! {
        pub(crate) fn #function_name() {
            let vec: &[&str] = &[
                #(#strings),*
            ];
            vec.into_iter().for_each(|string| {
                sid::SID::register(string);
            });
        }
    }
}
