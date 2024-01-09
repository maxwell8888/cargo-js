extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use ravascript_core::from_file;
use syn::{parse_macro_input, Block, ExprBlock, ItemFn, ItemMod, LitStr, File};

#[proc_macro]
pub fn include_ravascript(input: TokenStream) -> TokenStream {
    let path = parse_macro_input!(input as LitStr);
    let path_str = path.value();

    let rs =
        std::fs::read_to_string(&path_str).expect(&format!("Unable to read file: {}", path_str));
    let js = from_file(&rs, true)
        .iter()
        .map(|line| line.js_string())
        .collect::<Vec<_>>()
        .join("\n");

    let expanded = quote! {
        #js
    };

    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn file_as_str(_attr: TokenStream, file: TokenStream) -> TokenStream {
    let file_ast: File = parse_macro_input!(file as File);
    // let fn_name = &ast.sig.ident;
    // let fn_body = &ast.block;
    // let fn_name_code = format!("{}_code_str", fn_name);
    // let fn_name_code_ident = syn::Ident::new(&fn_name_code, fn_name.span());
    let code_string = format!("{}", quote! { #file_ast });

    let expanded = quote! {
        #file_ast

        pub fn file_code_str() -> &'static str {
            #code_string
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn fn_as_str(_attr: TokenStream, item: TokenStream) -> TokenStream {
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

#[proc_macro_attribute]
pub fn fn_stmts_as_str(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_fn: ItemFn = parse_macro_input!(item as ItemFn);
    let block = &*item_fn.block;
    let code_string = format!("{}", quote! { #block });

    let expanded = quote! {
        pub fn fn_code_str() {
            #block
        }

        pub fn block_code_str() -> &'static str {
            #code_string
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn module_as_str(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast: ItemMod = parse_macro_input!(item as ItemMod);
    // let mod_name = &ast.ident;
    // let fn_body = &ast.block;
    // let mod_name_code = format!("{}_code_str", mod_name);
    // let mod_name_code_ident = syn::Ident::new(&mod_name_code, mod_name.span());
    let code_string = format!("{}", quote! { #ast });

    let expanded = quote! {
        #ast

        pub fn generated_js() -> String {
            let generated_js = generate_js_from_module(#code_string);
            let generated_js = format_js(generated_js);
            generated_js
        }
    };

    TokenStream::from(expanded)
}
