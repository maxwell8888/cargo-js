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
async fn option_match() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        let result = match &five {
            Some(val) => *val,
            None => 0,
        };
        assert_eq!(result, 5);
    });

    // println!("{actual}");

    let expected = format_js(concat!(
        include_str!("option_prelude.js"),
        r#"var Some = Option.Some;
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
        "#,
    ));

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}

#[tokio::test]
async fn option_unwrap_or() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        assert_eq!(five.unwrap_or(0), 5);
        let nothing = None;
        assert_eq!(nothing.unwrap_or(0), 0);
    });

    let expected = format_js(concat!(
        include_str!("option_prelude.js"),
        r#"var Some = Option.Some;
        var None = Option.None;
        var five = Some(5);
        console.assert(five.unwrapOr(0) === 5);
        var nothing = None;
        console.assert(nothing.unwrapOr(0) === 0);
        "#,
    ));

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}

#[tokio::test]
async fn option_is_some_and() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        assert!(five.is_some_and(|x| x == 5));
    });

    let expected = format_js(concat!(
        include_str!("option_prelude.js"),
        r#"var Some = Option.Some;
        var five = Some(5);
        console.assert(five.isSomeAnd((x) => x === 5));
        "#,
    ));

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}
