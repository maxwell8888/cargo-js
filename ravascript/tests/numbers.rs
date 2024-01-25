mod stuff;
mod utils;
use pretty_assertions::{assert_eq, assert_ne};
use ravascript::prelude::web::JsNumber;
use ravascript::prelude::web::Math;
use ravascript::prelude::web::{
    try_, Console, Document, Event, HTMLInputElement, JsError, Json, Node, SyntaxError, NAVIGATOR,
};
use ravascript::prelude::*;
use ravascript::{catch, try_};
use ravascript::{format_js, from_block, from_crate, generate_js_from_module};
use ravascript_macros::module_as_str;
use ravascript_macros::{fn_as_str, fn_stmts_as_str};
use utils::*;

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
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_float_prelude.js"),
        r#"var num = new RustInteger(-5);
        var abs = num.abs();
        console.assert(abs.eq(new RustInteger(5)));
        var num = new RustFloat(-5.5);
        var abs = num.abs();
        console.assert(abs.eq(new RustFloat(5.5)));
        var num = new RustInteger(-5);
        var abs = num.abs();
        console.assert(abs.eq(new RustInteger(5)));
        var num = new RustFloat(-5.5);
        var abs = num.abs();
        console.assert(abs.eq(new RustFloat(5.5)));
        var num = -5;
        var abs = Math.abs(num);
        console.assert(abs === 5);
        var num = -5.5;
        var abs = Math.abs(num);
        console.assert(abs === 5.5);
        "#,
    ));

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}
