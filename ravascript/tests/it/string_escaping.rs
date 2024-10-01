use pretty_assertions::assert_eq;
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;

use super::utils::*;
use crate::r2j_block_unformatted;

#[tokio::test]
async fn string_escaping() {
    #[allow(unused_variables)]
    let actual = r2j_block_unformatted!({
        let json = r#"{ "double": 'single', `backtick`: \ }"#;
        let json2 = "{ \"double\": \'single\', `backtick`: \\ }";
        let json3 = "{ 'single': 'single', `backtick`: \\ }";
    });

    // NOTE we are intentionally not using format_js here to ensure the output string formatting is not altered
    let expected = r#"let json = "{ \"double\": \'single\', `backtick`: \\ }";
let json2 = "{ \"double\": \'single\', `backtick`: \\ }";
let json3 = "{ 'single': 'single', 'backtick': \\ }";"#;
    println!("{expected}");

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}
