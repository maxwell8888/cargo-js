use pretty_assertions::assert_eq;
use ravascript::prelude::web::{
    // try_, AnyHtmlElement, Console, Document, Event, HTMLInputElement, HtmlElement, JsError,
    Document,
    Event,
    HTMLInputElement,
    Node,
    NAVIGATOR,
};
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;
// use web_prelude::{HtmlAnyElement, HtmlElement};

use super::utils::*;
use crate::{r2j_block, r2j_block_with_prelude, r2j_file_no_rust, r2j_file_run_main};

#[tokio::test]
async fn json_parse_unsafe() {
    let actual = r2j_file_no_rust!(
        use web_prelude::{SyntaxError, catch, try_, json_parse};
        #[derive(Default)]
        pub struct Foo {
            text: String,
            num: i32,
        }

        fn main() {
            let json = r#"{ "text": "hello", "num": 5 }"#;
            // To acutally have a compiling, working json_parse implementation that we can run as Rust, not only would we need to include serde_json, but we would also need to derive `Deserialize` for Foo
            // TODO fix specifying generic in turbofish
            // let foo = unsafe { json_parse(json).cast::<Foo>() };
            let foo: Foo = unsafe { json_parse(json).cast() };
            assert!(foo.text == "hello");
            assert!(foo.num == 5);
        }
    );

    // NOTE the formatter is automatically converting the escaped JSON string to just use single quotes.
    let expected = format_js(
        r#"
            class Foo {
                constructor(text, num) {
                    this.text = text;
                    this.num = num;
                }
            }
            function main() {
                let json = '{ "text": "hello", "num": 5 }';
                let foo = (() => {
                    return JSON.parse(json);
                })();
                console.assert(foo.text === "hello");
                console.assert(foo.num === 5);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// #[ignore]
#[allow(dead_code, clippy::needless_return)]
#[tokio::test]
async fn json_parse_wrapper() {
    let actual = r2j_file_no_rust!(
        use web_prelude::{SyntaxError, catch, try_, json_parse};

        pub struct Foo {
            text: String,
            num: i32,
        }

        fn parse<T>(text: &str) -> Result<T, SyntaxError> {
            try_! {{
                // TODO remove iffe wrapper for blocks like this unsafe if the contents is a single expression
                return Ok(unsafe { json_parse(text).cast::<T>() });
            }}
            catch! {err, SyntaxError, {
                // return Err(err);
                return Err(err);
            }}
        }

        fn main() {
            let json = r#"{ "text": "hello", "num": 5 }"#;
            let foo = parse::<Foo>(json).unwrap();
            assert!(foo.text == "hello");
            assert!(foo.num == 5);
        }
    );

    // NOTE using this particular check for error types because it works even if the error originated in a different window/frame/etc, unlike `result instanceOf Error`. See https://stackoverflow.com/questions/30469261/checking-for-typeof-error-in-js
    // TODO maybe use instanceOf by default since it is cleaner, more idiomatic, and possibly more performant, and then fallback to toString if we detect that a new window or frame has been created?
    // NOTE we are assuming all exceptions are instances of `Error`. If someone throw a another type like `throw 5;` this will fail.
    let expected = format_js(
        r#"
            function resultUnwrap(result) {
                if (Object.prototype.toString.call(result) === "[object Error]") {
                    throw result;
                } else {
                    return result;
                }
            }

            class Foo {
                constructor(text, num) {
                    this.text = text;
                    this.num = num;
                }
            }
            function parse(text) {
                try {
                    return (() => {
                        return JSON.parse(text);
                    })();
                } catch (err) {
                    return err;
                }
            }
            function main() {
                let json = '{ "text": "hello", "num": 5 }';
                let foo = resultUnwrap(parse(json));
                console.assert(foo.text === "hello");
                console.assert(foo.num === 5);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// let tag_name = "button";
// let button: AnyHtmlElement = Document::create_element(tag_name);

#[allow(unused_variables)]
#[tokio::test]
async fn dom_nodes_and_elements() {
    setup_tracing();

    let actual = r2j_file_run_main!(
        use web_prelude::{HtmlDivElement, Document, document};
        fn main() {
            let div = document().create_element_div();
        }
    );

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

#[tokio::test]
async fn append_child() {
    setup_tracing();

    let actual = r2j_block_with_prelude!({
        use web_prelude::{document, Document, HtmlDivElement, Node};
        let div1 = document().create_element_div();
        let div2 = document().create_element_div();
        div1.append_child(div2);
    });

    let expected = format_js(
        r#"
            let div1 = document.createElement("div");
            let div2 = document.createElement("div");
            div1.appendChild(div2);
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn class_list() {
    let actual = r2j_block_with_prelude!({
        use web_prelude::{document, Document, Element, HtmlDivElement, Node};
        let div = document().create_element_div();
        div.class_list().add("red-border");
    });

    let expected = format_js(
        r#"
            let div = document.createElement("div");
            div.classList.add("red-border");
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn transmute_element_type() {
    let actual = r2j_block_with_prelude!({
        use web_prelude::{document, HtmlAnyElement, HtmlCanvasElement, HtmlElement};
        let any_canvas = document().create_element("canvas");
        any_canvas.focus();
        let typed_canvas =
            unsafe { std::mem::transmute::<HtmlAnyElement, HtmlCanvasElement>(any_canvas) };
        #[allow(unused_variables)]
        let context = typed_canvas.get_context("2d");
    });

    let expected = format_js(
        r#"
            let anyCanvas = document.createElement("canvas");
            anyCanvas.focus();
            let typedCanvas = (() => {
                return anyCanvas;
            })();
            let context = typedCanvas.getContext("2d");
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
