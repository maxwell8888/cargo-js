mod stuff;
mod utils;
use ravascript::prelude::web::{
    try_, Console, Document, Event, HTMLInputElement, JsError, Json, Node, SyntaxError, NAVIGATOR,
};
use ravascript::prelude::*;
use ravascript::{catch, try_};
use ravascript_core::{format_js, from_block, from_crate, generate_js_from_module};
use ravascript_macros::module_as_str;
use ravascript_macros::{fn_as_str, fn_stmts_as_str};
use utils::*;

#[tokio::test]
async fn it_transpiles_json_parse() {
    pub struct Foo {
        bar: usize,
    }
    let actual = r2j_block!({
        fn parse(text: &str) -> Result<Foo, SyntaxError> {
            try_! {{
                return Ok(Json::parse::<Foo>(text));
            }}
            catch! {err, SyntaxError,{
                return Err(err);
            }}
        }
    });

    let expected = r#"function parse(text) {
  try {
    return Ok(JSON.parse(text));
  } catch (err) {
    return Err(err);
  }
}"#;
    assert_eq!(expected, actual);
}

#[ignore = "navigator should be lower case so not a const"]
#[tokio::test]
async fn it_writes_to_clipboard() {
    let actual = r2j_block!({
        let input = Document::create_element2::<HTMLInputElement>("input");
        let button = Document::create_element("button");
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

// Check web "prelude" items are correctly inserted
#[tokio::test]
async fn option_null() {
    let actual = r2j_block_with_prelude!({
        let _five = Some(5);
    });

    let expected_js = format_js(
        r#"
        class Option {
            static noneId = "None";
            static None = {
                id: "None"
            };
            static someId = "Some";
            static Some(arg_0) {
                const data = {
                    id: "Some"
                };
                data.data = [arg_0];
                return data;
            }
        }

        var Some = MyEnum.Some;
        var _five = Some(5);
        "#,
    );

    assert_eq!(expected_js, actual);
}
