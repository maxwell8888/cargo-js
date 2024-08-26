#![allow(unused_imports)]

use pretty_assertions::assert_eq;
use ravascript::{
    catch,
    prelude::{
        web::{
            try_, AnyHtmlElement, Console, Document, Event, HTMLInputElement, HtmlElement, JsError,
            Json, Node, SyntaxError, NAVIGATOR,
        },
        *,
    },
    try_,
};
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;

use super::utils::*;
use crate::{r2j_block, r2j_file_run_main};

#[ignore]
#[allow(dead_code, clippy::needless_return)]
#[tokio::test]
async fn it_transpiles_json_parse() {
    let actual = r2j_file_run_main!(
        use web_prelude::{SyntaxError, catch, try_, Json};
        pub struct Foo {
            bar: i32,
        }
        fn parse(text: &str) -> Result<Foo, SyntaxError> {
            try_! {{
                return Ok(Json::parse::<Foo>(text));
            }}
            catch! {err, SyntaxError,{
                return Err(err);
            }}
        }
        fn main() {}
    );

    let expected = format_js(
        r#"
        function parse(text) {
            try {
                return Ok(JSON.parse(text));
            } catch (err) {
                return Err(err);
            }
        }
        "#,
    );  
    assert_eq!(expected, actual);
}

#[allow(unused_variables)]
#[tokio::test]
async fn dom_nodes_and_elements() {
    setup_tracing();

    let actual = r2j_file_run_main!(
        // use ravascript::prelude::web::{AnyHtmlElement, Document, HTMLInputElement};
        // let tag_name = "button";
        // let button: AnyHtmlElement = Document::create_element(tag_name);
        // let input: HTMLInputElement = Document::create_element("input");
        // // TODO make HTMLDivElement
        // let div: HTMLInputElement = Document::create_element("div");
        // div.append_child(input);
        // div.append_child(button);
        use web_prelude::{HtmlDivElement, Document, document}; 
        // let tag_name = "button";
        fn main() {
            let div = document().create_element_div();  
        }
        // div.append_child(input);
        // div.append_child(button);
    );

    // let expected = format_js(
    //     r#"
    //     let tagName = "button";
    //     let button = document.createElement(tagName);
    //     let input = document.createElement("input");
    //     let div = document.createElement("div");
    //     div.appendChild(input);
    //     div.appendChild(button);
    //     "#,
    // );
    let expected = format_js(
        r#"
        function main() {
            let div = document.createElement("div");
        }

        main();
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "reason"]
#[allow(unused_variables)]
#[tokio::test]
async fn append_child() {
    setup_tracing();

    let actual = r2j_file_run_main!(
        use web_prelude::{HtmlDivElement, Document, document, Node}; 
        fn main() {
            let div1 = document().create_element_div();  
            let div2 = document().create_element_div();  
            div1.append_child(div2);
        }
        // div.append_child(input);
        // div.append_child(button);
    );

    // let expected = format_js(
    //     r#"
    //     let tagName = "button";
    //     let button = document.createElement(tagName);
    //     let input = document.createElement("input");
    //     let div = document.createElement("div");
    //     div.appendChild(input);
    //     div.appendChild(button);
    //     "#,
    // );
    let expected = format_js(
        r#"
        function main() {
            let div = document.createElement("div");
        }

        main();
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "navigator should be lower case so not a const"]
#[tokio::test]
async fn it_writes_to_clipboard() {
    let actual = r2j_block!({
        let input = Document::create_element::<HTMLInputElement>("input");
        let button = Document::create_element_untyped("button");
        let get_text = |_event: Event| async { NAVIGATOR.clipboard.write_text(input.value).await };
        button.add_event_listener_async("click", get_text);
    });

    let expected_js = format_js(
        r#"
        let input = document.createElement("input");
        let button = document.createElement("button");
        let getText = async (_event) => await navigator.clipboard.writeText(input.value);
        button.addEventListener("click", getText);
        "#,
    );

    assert_eq!(expected_js, actual);
}
