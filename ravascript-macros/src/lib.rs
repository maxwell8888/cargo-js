extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn, LitStr};

#[proc_macro]
pub fn include_ravascript(input: TokenStream) -> TokenStream {
    let path = parse_macro_input!(input as LitStr);
    let path_str = path.value();

    let file_contents =
        std::fs::read_to_string(&path_str).expect(&format!("Unable to read file: {}", path_str));

    let expanded = quote! {
        #file_contents
    };

    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn item_as_str(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast: ItemFn = parse_macro_input!(item as ItemFn);
    let fn_name = &ast.sig.ident;
    // let fn_body = &ast.block;
    let fn_name_code = format!("{}_code_str", fn_name);
    let fn_name_code_ident = syn::Ident::new(&fn_name_code, fn_name.span());
    let code_string = format!("{}", quote! { #ast });

    let expanded = quote! {
        #ast

        pub fn #fn_name_code_ident() -> &'static str {
            #code_string
        }
    };

    TokenStream::from(expanded)
}
