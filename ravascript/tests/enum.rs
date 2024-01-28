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
async fn it_transpiles_enum_match() {
    // TODO use js file preludes here so we don't have to update them in enum_match.js also
    let (expected, actual) = get_rust_module_and_expected_js("tests/stuff/enum_match".into())
        .await
        .unwrap();

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}
