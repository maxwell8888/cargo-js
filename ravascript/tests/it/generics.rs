use pretty_assertions::assert_eq;
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;

use super::utils::*;
use crate::{r2j_block, r2j_block_with_prelude, r2j_file_run_main};

#[ignore = "TODO LOW PRIORITY"]
#[tokio::test]
async fn generic_into_concrete() {
    setup_tracing();
    let actual = r2j_block_with_prelude!({
        trait HasValue {
            fn value() -> Self;
        }
        impl HasValue for i32 {
            fn value() -> Self {
                1
            }
        }
        impl HasValue for i64 {
            fn value() -> Self {
                2
            }
        }
        fn get_t<T: HasValue>() -> T {
            // Like ::default()
            T::value()
        }
        let t_is_int1 = get_t::<i32>();
        let t_is_int2 = get_t::<i64>();
        // Whilst t_is_int* is in theory just a T so this would get transpiled to `tIsInt1.eq(tIsInt2)`, in this case we actually can determine that they are `Number`s so can just use `tIsInt1 === tIsInt2` instead
        assert!(t_is_int1 as i64 == t_is_int2);
    });

    let expected = format_js(
        r#"
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}
