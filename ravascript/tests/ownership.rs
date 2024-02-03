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

#[ignore = "wip"]
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
            num.jsNumber = new RustInteger(1);
            console.assert(num.eq(new RustInteger(1)).jsBoolean);
            return num;
        }
        var origNum = new RustInteger(0);
        console.assert(origNum.eq(new RustInteger(0)).jsBoolean);
        {
            var result = addOne(origNum);
            console.assert(result.eq(new RustInteger(1)).jsBoolean);
            var resultCopy = result.jsNumber;
            console.assert(resultCopy.eq(new RustInteger(1)).jsBoolean);
            result.add(new RustInteger(1));
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

    // assert_eq!(format_js(expected), actual);
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
        let mut wago = 5;
        fn checky(num: &mut i32) -> &mut i32 {
            *num += 1;
            num
        }
        let twoby = checky(&mut wago);
        *twoby += 1;
        dbg!(twoby);
        dbg!(wago);

        let mut chug = 4;
        let mut chan = chug;
        chan += 1;
        dbg!(chan);

        let woot = &mut 5;
        *woot += 1;
        dbg!(&woot);
        let woot2 = woot;
        // dbg!(&woot);

        fn add_one(num: &mut i32) -> &mut i32 {
            assert_eq!(*num, 0);
            *num = 1;
            assert_eq!(*num, 1);
            num
        }

        struct Thing<'a> {
            numy: &'a mut i32,
        }
        let mut valy = 5;
        let mut cool = Thing { numy: &mut valy };
        cool.numy = &mut 2;
        let mut orig_num = 0;
        assert_eq!(orig_num, 0);

        {
            let result = add_one(&mut orig_num);
            assert_eq!(*result, 1);
            *result += 1;
            assert_eq!(*result, 2);
        }

        assert_eq!(orig_num, 2);

        orig_num += 1;
        assert_eq!(orig_num, 3);
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
