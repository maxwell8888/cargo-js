use pretty_assertions::assert_eq;
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;

use super::utils::*;
use crate::r2j_block_with_prelude;

#[tokio::test]
async fn mutating_strings() {
    setup_tracing();
    let actual = r2j_block_with_prelude!({
        fn add_one(text: &mut String) -> &mut String {
            assert!(*text == "one");
            *text = "two".to_string();
            assert!(*text == "two");
            text
        }
        let mut orig_text = "one".to_string();
        {
            let mut result = add_one(&mut orig_text);
            assert!(*result == "two");
            // TODO what if result was a ref to a struct (copy or move)?
            let result_copy = result.clone();
            assert!(result_copy == "two");
            *result += "three";
            assert!(result_copy == "two");
            assert!(*result == "twothree");
            let four = &mut "four".to_string();
            // TODO can't use `&mut 6` after this because it is single ownership so `*result = 6` and `result = &mut 6` can be handled/parsed the same
            result = four;
            assert!(*result == "four");
        }
        assert!(orig_text == "twothree");
        orig_text.push_str("four");
        assert!(orig_text == "twothreefour");
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
        let origText = new RustString("one");
        {
            let result = addOne(origText);
            console.assert(result.inner === "two");
            let resultCopy = result.inner;
            console.assert(resultCopy === "two");
            result.inner += "three";
            console.assert(resultCopy === "two");
            console.assert(result.inner === "twothree");
            let four = new RustString("four");
            result = four;
            console.assert(result.inner === "four");
        }
        console.assert(origText.inner === "twothree");
        origText.inner += "four";
        console.assert(origText.inner === "twothreefour");
        "#
    ));

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}
