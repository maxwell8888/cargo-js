mod utils;
use pretty_assertions::{assert_eq, assert_ne};
use ravascript::prelude::web::{
    try_, AnyHtmlElement, Console, Document, Event, HTMLInputElement, HtmlElement, JsError, Json,
    Node, SyntaxError, NAVIGATOR,
};
use ravascript::prelude::*;
use ravascript::{catch, try_};
use ravascript_core::{format_js, from_block, from_crate, generate_js_from_module};
use ravascript_macros::module_as_str;
use ravascript_macros::{fn_as_str, fn_stmts_as_str};
use utils::*;

#[ignore]
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
    println!("{actual}");
    assert_eq!(expected, actual);
}

// #[ignore = "reason"]
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
        use web_prelude::{Div, Document};
        // let tag_name = "button";
        fn main() {
            let div = Document::create_element("div");
        }
        // div.append_child(input);
        // div.append_child(button);
    );

    // let expected = format_js(
    //     r#"
    //     var tagName = "button";
    //     var button = document.createElement(tagName);
    //     var input = document.createElement("input");
    //     var div = document.createElement("div");
    //     div.appendChild(input);
    //     div.appendChild(button);
    //     "#,
    // );
    let expected = format_js(
        r#"
        function main() {
            var div = document.createElement("div");
        }

        main();
        "#,
    );

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
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
        var input = document.createElement("input");
        var button = document.createElement("button");
        var getText = async (_event) => await navigator.clipboard.writeText(input.value);
        button.addEventListener("click", getText);
        "#,
    );

    assert_eq!(expected_js, actual);
}
