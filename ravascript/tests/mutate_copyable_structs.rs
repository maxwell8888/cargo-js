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

#[ignore]
#[tokio::test]
async fn ownership_mut2() {
    let actual = r2j_block_with_prelude!({
        struct Thing<'a> {
            numy: &'a mut i32,
        }
        let mut valy = 5;
        let mut cool = Thing { numy: &mut valy };
        cool.numy = &mut 2;
    });

    let expected = concat!(
        include_str!("option_prelude.js"),
        "var Some = Option.Some;",
        "var None = Option.None;",
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"var counter = new RustInteger(0);
        var someNum = Some(new RustInteger(5));
        if (someNum.id === Option.someId) {
            var [num] = someNum.data;
            counter += num;
        } else {
            counter += new RustInteger(1);
        }
        console.assert(counter.eq(new RustInteger(5)));
        if (None.id === Option.someId) {
            var [num] = someNum.data;
            counter += num;
        } else {
            counter += new RustInteger(1);
        }
        console.assert(counter.eq(new RustInteger(6)));
        "#
    );

    // assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore]
#[tokio::test]
async fn ownership_copy_struct() {
    let actual = r2j_block_with_prelude!({
        struct Thing<'a> {
            numy: &'a mut i32,
        }
        let mut valy = 5;
        let mut cool = Thing { numy: &mut valy };
        cool.numy = &mut 2;
    });

    let expected = concat!(
        include_str!("option_prelude.js"),
        "var Some = Option.Some;",
        "var None = Option.None;",
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"var counter = new RustInteger(0);
        var someNum = Some(new RustInteger(5));
        if (someNum.id === Option.someId) {
            var [num] = someNum.data;
            counter += num;
        } else {
            counter += new RustInteger(1);
        }
        console.assert(counter.eq(new RustInteger(5)).jsBoolean);
        if (None.id === Option.someId) {
            var [num] = someNum.data;
            counter += num;
        } else {
            counter += new RustInteger(1);
        }
        console.assert(counter.eq(new RustInteger(6)).jsBoolean);
        "#
    );

    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}