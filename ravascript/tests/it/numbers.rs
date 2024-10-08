use pretty_assertions::assert_eq;
use ravascript::{
    format_js,
    prelude::{web::Math, JsNumber},
};
use ravascript_macros::fn_stmts_as_str;

use super::utils::*;
use crate::r2j_block_with_prelude;

#[tokio::test]
async fn simple_integer_var() {
    let actual = r2j_block_with_prelude!({
        let num: i32 = 1;
        assert!(num == 1);
    });

    let expected = format_js(
        r#"
            let num = 1;
            console.assert(num === 1);
        "#,
    );

    execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}

#[ignore]
#[tokio::test]
async fn abs() {
    let actual = r2j_block_with_prelude!({
        // TODO non mut ref numbers should not be wrapper in RustInteger and RustFloat
        // TODO 5. being transpiled to 5.0
        // normal use of rust numbers
        // int
        let num: i32 = -5;
        let abs = num.abs();
        assert_eq!(abs, 5);
        // float
        let num: f32 = -5.5;
        let abs = num.abs();
        assert_eq!(abs, 5.5);

        // normal use of mut ref rust numbers
        // int
        let num: &mut i32 = &mut -5;
        let abs = num.abs();
        assert_eq!(abs, 5);
        // float
        let num: &mut f32 = &mut -5.5;
        let abs = num.abs();
        assert_eq!(abs, 5.5);

        // Use JS methods directly
        // int
        let num = JsNumber(-5);
        let abs = Math::abs(num);
        assert_eq!(abs, JsNumber(5));
        // float
        let num = JsNumber(-5.5);
        let abs = Math::abs(num);
        assert_eq!(abs, JsNumber(5.5));
    });

    let expected = format_js(concat!(
        // include_str!("option_prelude.js"),
        include_str!("../rust_integer_prelude.js"),
        include_str!("../rust_float_prelude.js"),
        r#"let num = -5;
        let abs = num.abs();
        console.assert(abs.eq(5));
        let num = new RustFloat(-5.5);
        let abs = num.abs();
        console.assert(abs.eq(5.5));
        let num = new RustInteger(-5);
        let abs = num.abs();
        console.assert(abs.eq(5));
        let num = new RustFloat(-5.5);
        let abs = num.abs();
        console.assert(abs.eq(5.5));
        let num = -5;
        let abs = Math.abs(num);
        console.assert(abs === 5);
        let num = -5.5;
        let abs = Math.abs(num);
        console.assert(abs === 5.5);
        "#,
    ));

    execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}
