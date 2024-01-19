mod stuff;
mod utils;
use pretty_assertions::{assert_eq, assert_ne};
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
        let five = Some(5);
        let result = match &five {
            Some(val) => *val,
            None => 0,
        };
        assert_eq!(result, 5);
        assert_eq!(five.unwrap_or(0), 5);
        let nothing = None;
        assert_eq!(nothing.unwrap_or(0), 0);
    });

    let expected = format_js(
        r#"
        class Option {
            static someId = "Some";
            static noneId = "None";
            static None = new Option("None", null);
            constructor(id, data) {
                this.id = id;
                this.data = data;
            }
            static Some(arg_0) {
                return new Option("Some", [arg_0]);
            }
            unwrapOr(defaultVzxyw) {
                var ifTempAssignment;
                if (this.id === Option.someId) {
                    var [x] = this.data;
                    ifTempAssignment = x;
                } else if (this.id === Option.noneId) {
                    ifTempAssignment = defaultVzxyw;
                } else {
                    ifTempAssignment = "this shouldn't exist";
                }
                return ifTempAssignment;
            }
        }
        var Some = Option.Some;
        var None = Option.None;
        var five = Some(5);
        var result;
        if (five.id === Option.someId) {
            var [val] = five.data;
            result = val;
        } else if (five.id === Option.noneId) {
            result = 0;
        } else {
            result = "this shouldn't exist";
        }
        console.assert(result === 5);
        console.assert(five.unwrapOr(0) === 5);
        var nothing = None;
        console.assert(nothing.unwrapOr(0) === 0);
        "#,
    );

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    // assert_eq!(expected_js, actual);
}
