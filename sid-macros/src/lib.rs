use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, Expr};


/// Generates an iterator for SIDs based on a list of string literals
/// 
/// # Examples
/// 
/// ```
/// use sid::sid_iter;
/// 
/// let state_ids = sid_iter!["Idle", "Walking", "Running", "Jumping"];
/// for state_id in state_ids {
///    println!("State: {}", state_id);
/// }
/// ```
#[proc_macro]
pub fn sid_iter(tokens: TokenStream) -> TokenStream {
    let expr = parse_macro_input!(tokens as Expr);
    
    match expr {
        Expr::Array(array) => {
            let elements = array.elems.iter().map(|e| match e {
                Expr::Lit(lit) => {
                    if let syn::Lit::Str(str_lit) = &lit.lit {
                        let string_value = str_lit.value();
                        quote! { ::sid::SID::from(#string_value) }
                    } else {
                        syn::Error::new_spanned(
                            e,
                            "sid_iter expects string literals",
                        )
                        .to_compile_error()
                    }
                }
                _ => syn::Error::new_spanned(
                    e,
                    "sid_iter expects an array of string literals",
                )
                .to_compile_error(),
            });

            quote! {
                [#(#elements),*]
            }
            .into()
        }
        _ => syn::Error::new_spanned(
            expr.to_token_stream(),
            "sid_iter expects an array of string literals",
        )
        .to_compile_error()
        .into(),
    }
}

//struct LitStrCollector(Vec<String>);

//impl<'ast> Visit<'ast> for LitStrCollector {
//    fn visit_lit_str(&mut self, lit: &'ast LitStr) {
//        self.0.push(lit.value());
//    }
//}

//pub(crate) fn sid_iter_internal(input: TokenStream, attrs: TokenStream) -> TokenStream2 {
//    if let Ok(module_parsed) = syn::parse(input.clone()) {
//        parse_mod(module_parsed, attrs)
//    } else if let Ok(impl_block_parsed) = syn::parse::<ItemImpl>(input) {
//        parse_item_impl(impl_block_parsed, attrs)
//    } else {
//        return quote! { compile_error!("sid_iter needs to annotate either an impl block or a module")}.into();
//    }
//}
//
//fn parse_item_impl(mut item: ItemImpl, attrs: TokenStream) -> TokenStream2 {
//    let mut collector = LitStrCollector(Vec::new());
//    collector.visit_item_impl(&item);
//    item.items.push(syn::ImplItem::Verbatim(sid_register_tokens(collector.0, attrs)));
//    item.to_token_stream()
//}
//
//fn parse_mod(mut item: ItemMod, attrs: TokenStream) -> TokenStream2 {
//    let mut collector = LitStrCollector(Vec::new());
//    collector.visit_item_mod(&item);
//    if let Some((_, ref mut content)) = item.content {
//        let func = sid_register_tokens(collector.0, attrs);
//        content.push(Item::Verbatim(func));
//    } else {
//        return quote! { compile_error!("sid_iter needs to annotate mod items that directly contain other items"); };
//    }
//    item.to_token_stream()
//}
//
//fn sid_register_tokens(strings: Vec<String>, attrs: TokenStream) -> TokenStream2 {
//    let function_name = if attrs.is_empty() {
//        Ident::new("sid_register", Span2::call_site())
//    } else if let Ok(function_name) = syn::parse_str::<Ident>(&attrs.to_string()) {
//        function_name
//    } else {
//        return quote! { compile_error!("#attrs cannot be parsed as a valid Rust identifier")};
//    };
//    
//    quote! {
//        pub(crate) fn #function_name() {
//            let vec: &[&str] = &[
//                #(#strings),*
//            ];
//            vec.into_iter().for_each(|string| {
//                sid::SID::register(string);
//            });
//        }
//    }
//}
