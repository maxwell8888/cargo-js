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
            assert_eq!(*text, "one");
            *text = "two".to_string();
            assert_eq!(*text, "two");
            text
        }
        let mut orig_text = "one".to_string();
        {
            let mut result = add_one(&mut orig_text);
            assert_eq!(*result, "two");
            // TODO what if result was a ref to a struct (copy or move)?
            let result_copy = result.clone();
            assert_eq!(result_copy, "two");
            *result += "three";
            assert_eq!(result_copy, "two");
            assert_eq!(*result, "twothree");
            let four = &mut "four".to_string();
            // TODO can't use `&mut 6` after this because it is single ownership so `*result = 6` and `result = &mut 6` can be handled/parsed the same
            result = four;
            assert_eq!(*result, "four");
        }
        assert_eq!(orig_text, "twothree");
        orig_text.push_str("four");
        assert_eq!(orig_text, "twothreefour");
    });

    let expected = format_js(concat!(
        include_str!("rust_string_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"function addOne(text) {
            console.assert(text.eq("one"));
            text.derefAssign("two");
            console.assert(text.eq("two"));
            return text;
        }
        var origText = new RustString("one");
        {
            var result = addOne(origText);
            console.assert(result.eq("two"));
            var resultCopy = result.clone();
            console.assert(resultCopy.eq("two"));
            result.addAssign("three");
            console.assert(resultCopy.eq("two"));
            console.assert(result.eq("twothree"));
            var four = new RustString("four");
            result = four;
            console.assert(result.eq("four"));
        }
        console.assert(origText.eq("twothree"));
        origText.pushStr("four");
        console.assert(origText.eq("twothreefour"));
        "#
    ));

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
