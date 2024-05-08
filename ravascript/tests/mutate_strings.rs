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
async fn mutating_strings() {
    let actual = r2j_block_with_prelude!({
        fn add_one(text: &mut String) -> &mut String {
            assert!(*text == "one");
            *text = "two".to_string();
            assert!(*text == "two");
            text
        }
        // let mut orig_text = "one".to_string();
        // {
        //     let mut result = add_one(&mut orig_text);
        //     assert!(*result == "two");
        //     // TODO what if result was a ref to a struct (copy or move)?
        //     let result_copy = result.clone();
        //     assert!(result_copy == "two");
        //     *result += "three";
        //     assert!(result_copy == "two");
        //     assert!(*result == "twothree");
        //     let four = &mut "four".to_string();
        //     // TODO can't use `&mut 6` after this because it is single ownership so `*result = 6` and `result = &mut 6` can be handled/parsed the same
        //     result = four;
        //     assert!(*result == "four");
        // }
        // assert!(orig_text == "twothree");
        // orig_text.push_str("four");
        // assert!(orig_text == "twothreefour");
    });

    // include_str!("rust_string_prelude.js"),
    // include_str!("rust_bool_prelude.js"),
    let expected = format_js(concat!(
        "class RustString {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"function addOne(text) {
            console.assert(text.inner === "one");
            text.inner = "two";
            console.assert(text.inner === "two");
            return text;
        }
        var origText = new RustString("one");
        {
            var result = addOne(origText);
            console.assert(result.inner === "two");
            var resultCopy = result.inner;
            console.assert(resultCopy === "two");
            result.inner += "three";
            console.assert(resultCopy === "two");
            console.assert(result.inner === "twothree");
            var four = new RustString("four");
            result = four;
            console.assert(result.inner === "four");
        }
        console.assert(origText.inner === "twothree");
        origText.inner += "four";
        console.assert(origText.inner === "twothreefour");
        "#
    ));

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
