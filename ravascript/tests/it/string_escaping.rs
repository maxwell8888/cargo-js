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
        // TODO note that ideally this would transpile to `let json3 = "{ \"double\": \'single\', `backtick`: \\ }";` ie allow users to preserve the escaping they used in the Rust string, but for now the approach the handling of ExprRef::Lit takes is to use `.values()` which has all backslashes removed and then add backslahes back in but only for double speech marks.
        let json2 = "{ \"double\": \'single\', `backtick`: \\ }";
        let json3 = "{ 'single': 'single', `backtick`: \\ }";
    });

    // NOTE we are intentionally not using format_js here to ensure the output string formatting is not altered
    // NOTE the devtools console will confusingly print eg '\'' as "'" but '\\' as '\\' even though '\\' does correctly evaluate to a single backslash (ie if you assign the value to a div textContent). So using \\ for a single backslash in a string is correct.
    let expected = r#"let json = "{ \"double\": 'single', `backtick`: \\ }";
let json2 = "{ \"double\": 'single', `backtick`: \\ }";
let json3 = "{ 'single': 'single', `backtick`: \\ }";"#;

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn char_escaping() {
    #[allow(unused_variables)]
    let actual = r2j_block_unformatted!({
        let double = '"';
        let escaped_double = '\"';
        let escaped_single = '\'';
        let backtick = '`';
        let escaped_backslash = '\\';
    });

    // NOTE we are intentionally not using format_js here to ensure the output string formatting is not altered
    let expected = r#"let double = '"';
let escapedDouble = '"';
let escapedSingle = '\'';
let backtick = '`';
let escapedBackslash = '\\';"#;

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}
