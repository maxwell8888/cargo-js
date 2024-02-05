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
async fn mutating_integers() {
    let actual = r2j_block_with_prelude!({
        /// increments a mutable reference to an int
        fn add_one(num: &mut i32) -> &mut i32 {
            assert_eq!(*num, 0);
            *num = 1;
            assert_eq!(*num, 1);
            num
        }
        let mut orig_num = 0;
        assert_eq!(orig_num, 0);
        {
            let mut result = add_one(&mut orig_num);
            assert_eq!(*result, 1);
            // TODO what if result was a ref to a struct (copy or move)?
            let result_copy = *result;
            assert_eq!(result_copy, 1);
            *result += 1;
            assert_eq!(result_copy, 1);
            assert_eq!(*result, 2);
            let six = &mut 6;
            // TODO can't use `&mut 6` after this because it is single ownership so `*result = 6` and `result = &mut 6` can be handled/parsed the same
            result = six;
            assert_eq!(*result, 6);
        }
        assert_eq!(orig_num, 2);
        orig_num += 1;
        assert_eq!(orig_num, 3);
    });

    let expected = concat!(
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"function addOne(num) {
            console.assert(num.eq(new RustInteger(0)).jsBoolean);
            num.derefAssign(new RustInteger(1));
            console.assert(num.eq(new RustInteger(1)).jsBoolean);
            return num;
        }
        var origNum = new RustInteger(0);
        console.assert(origNum.eq(new RustInteger(0)).jsBoolean);
        {
            var result = addOne(origNum);
            console.assert(result.eq(new RustInteger(1)).jsBoolean);
            var resultCopy = result.copy();
            console.assert(resultCopy.eq(new RustInteger(1)).jsBoolean);
            result.addAssign(new RustInteger(1));
            console.assert(resultCopy.eq(new RustInteger(1)).jsBoolean);
            console.assert(result.eq(new RustInteger(2)).jsBoolean);
            var six = new RustInteger(6);
            result = six;
            console.assert(result.eq(new RustInteger(6)).jsBoolean);
        }
        console.assert(origNum.eq(new RustInteger(2)).jsBoolean);
        origNum.addAssign(new RustInteger(1));
        console.assert(origNum.eq(new RustInteger(3)).jsBoolean);
        "#
    );

    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn deref_vs_normal_assign() {
    let actual = r2j_block_with_prelude!({
        let mut result = &1;
        assert_eq!(*result, 1);
        let result_copy = *result;
        assert_eq!(result_copy, 1);
        let mut six = &6;
        // TODO can't use six after this because it is single ownership, but try similar thing with &6 to understand *result = 1 vs result = &1
        result = six;
        assert_eq!(*result, 6);
        six = &10;
        assert_eq!(*six, 10);
        assert_eq!(*result, 6);
    });

    let expected = concat!(
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"var result = new RustInteger(1);
        console.assert(result.eq(new RustInteger(1)).jsBoolean);
        var resultCopy = result;
        console.assert(resultCopy.eq(new RustInteger(1)).jsBoolean);
        var six = new RustInteger(6);
        result = six;
        console.assert(result.eq(new RustInteger(6)).jsBoolean);
        six = new RustInteger(10);
        console.assert(six.eq(new RustInteger(10)).jsBoolean);
        console.assert(result.eq(new RustInteger(6)).jsBoolean);
        "#
    );

    assert_eq!(format_js(expected), actual);
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

    // assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore]
#[tokio::test]
async fn ownership_mut() {
    let actual = r2j_block_with_prelude!({
        let mut foo = 5;
        fn add_one(num: &mut i32) -> &mut i32 {
            *num += 1;
            num
        }
        let bar = add_one(&mut foo);
        *bar += 1;
        assert_eq!(*bar, 6);
        assert_eq!(foo, 6);

        let mut baz = 4;
        let mut baz_two = baz;
        baz_two += 1;
        assert_eq!(baz, 5);
        assert_eq!(baz_two, 5);

        let five = &mut 5;
        *five += 1;
        let six = five;
        assert_eq!(*six, 6);
    });

    let expected = concat!(
        include_str!("option_prelude.js"),
        "var Some = Option.Some;",
        "var None = Option.None;",
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"var foo = new RustInteger(5);
        function addOne(num) {
            num.addAssign(new RustInteger(1));
            return num;
        }
        var bar = addOne(foo);
        bar.addAssign(new RustInteger(1));
        console.assert(bar.eq(new RustInteger(6)));
        console.assert(foo.eq(new RustInteger(6)));
        var baz = new RustInteger(5);
        var baz_two = baz;
        baz_two.addAssign(new RustInteger(1));
        console.assert(baz.eq(new RustInteger(5)));
        console.assert(baz_two.eq(new RustInteger(5)));
        var five = new RustInteger(5);
        five.addAssign(new RustInteger(1));
        var six = five;
        console.assert(six.eq(new RustInteger(6)));
        "#
    );

    // assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

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
