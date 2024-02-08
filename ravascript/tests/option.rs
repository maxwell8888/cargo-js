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
        "var Some = Option.Some;",
        r#"var five = Some(new RustInteger(5));
        var result;
        if (five.id === Option.someId) {
            var [val] = five.data;
            result = val;
        } else if (five.id === Option.noneId) {
            result = 0;
        } else {
            throw new Error("couldn't match enum variant");
        }
        console.assert(result.eq(5));
        "#,
    ));

    // println!("{expected}");

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
        "var Some = Option.Some;",
        r#"var five = Some(5);
        console.assert(five.isSomeAnd((x) => x.eq(5)));
        "#,
    ));

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}

#[tokio::test]
async fn option_unwrap() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        assert_eq!(five.unwrap(), 5);
    });

    let expected = format_js(concat!(
        include_str!("option_prelude.js"),
        "var Some = Option.Some;",
        r#"var five = Some(5);
        console.assert(five.unwrap().eq(5));
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
        "var Some = Option.Some;
        var None = Option.None;",
        "var five = Some(5);
        console.assert(five.unwrapOr(0).eq(5));
        var nothing = None;
        console.assert(nothing.unwrapOr(0).eq(0));
        ",
    ));

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}

#[tokio::test]
async fn option_unwrap_or_else() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        let none: Option<i32> = None;
        assert_eq!(five.unwrap_or_else(|| 4), 5);
        assert_eq!(none.unwrap_or_else(|| 4), 4);
    });

    let expected = format_js(concat!(
        include_str!("option_prelude.js"),
        "var Some = Option.Some;
        var None = Option.None;",
        "var five = Some(5);
        var none = None;
        console.assert(five.unwrapOrElse(() => 4).eq(5));
        console.assert(none.unwrapOrElse(() => 4).eq(4));
        ",
    ));

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}

// TODO unwrap_or_default uses T::default() which means we would need to know the type of five or none so we can convert it to eg i32.default() ???
#[ignore = "needs to know type of instance"]
#[tokio::test]
async fn option_unwrap_or_default() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        let none: Option<i32> = None;
        assert_eq!(five.unwrap_or_default(), 5);
        assert_eq!(none.unwrap_or_default(), 0);
    });

    let expected = format_js(concat!(
        include_str!("option_prelude.js"),
        "var Some = Option.Some;
        var None = Option.None;",
        r#"var five = Some(5);
        var none = None;
        console.assert(five.unwrapOrElse(() => 4).eq(5));
        console.assert(none.unwrapOrElse(() => 4).eq(4));
        "#,
    ));

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}

#[tokio::test]
async fn option_map() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        let none: Option<i32> = None;
        assert_eq!(five.map(|x| x + 2), Some(7));
        assert_eq!(none.map(|x| x + 2), None);
        let _ = 5.eq(&4);
    });

    // println!("{actual}");

    // TODO this won't pass because we need to impl deep copies for JS
    let expected = format_js(concat!(
        include_str!("option_prelude.js"),
        "var Some = Option.Some;",
        "var None = Option.None;",
        r#"var five = Some(5);
        var none = None;
        console.assert(five.map((x) => x.add(2)).eq(Some(7)));
        console.assert(none.map((x) => x.add(2)).eq(None));
        var _ = 5.eq(4);
        "#,
    ));

    // let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}
