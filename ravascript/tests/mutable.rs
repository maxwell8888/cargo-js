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
async fn mutate_int() {
    let actual = r2j_block_with_prelude!({
        let mut num = 0;
        num += 1;
        assert_eq!(num, 1);
    });

    let expected = concat!(
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"var num = new RustInteger(0);
        num.addAssign(new RustInteger(1));
        console.assert(num.eq(new RustInteger(1)).jsBoolean)"#
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "needs num to be copied"]
#[tokio::test]
async fn mutate_int_fn_arg() {
    let actual = r2j_block_with_prelude!({
        let orig_num = 0;
        assert_eq!(orig_num, 0);
        fn add_one(mut num: i32) -> i32 {
            assert_eq!(num, 0);
            num += 1;
            assert_eq!(num, 1);
            num
        }
        let result = add_one(orig_num);
        assert_eq!(result, 1);
        assert_eq!(orig_num, 0);
    });

    let expected = format_js(concat!(
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"var origNum = new RustInteger(0);
        console.assert(origNum.eq(new RustInteger(0)).jsBoolean);
        function addOne(num) {
            console.assert(num.eq(new RustInteger(0)).jsBoolean);
            num.addAssign(new RustInteger(1));
            console.assert(num.eq(new RustInteger(1)).jsBoolean);
            return num;
        }
        var result = addOne(origNum);
        console.assert(result.eq(new RustInteger(1)).jsBoolean);
        console.assert(origNum.eq(new RustInteger(0)).jsBoolean);
        "#
    ));
    println!("{expected}");
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn mut_ref_int_fn_arg() {
    // whilst it might appear that you can mutate numbers in JS with `var x = 0; x++;`, `=`, `++`, and `+=` just reassign a value to the variable, and when we pass the variable to a function the value is copied to a new variable, and of course we are not able to reassign values to the original variable within the funciton, only the new variable.
    // A problem is that when create eg `var num = 5; var mutRef = { rustDeref: num };`, we are copying num, so if we mutate mutRef, num doesn't get updated. So I think we need ensure all mut numbers are wrapped. but how will we know how to handle `num = 4;`? Well if it is a reassignment then we know num must be mut so rhs needs wrapping? But we don't know the type of num, so it means everything gets wrapper. Whilst this seems redundant because eg an object is already mutable so doesn't need wrapping, they will actually need wrapping to support calling `.rustDeref()`. But this means if we do `num = 4; num = num; num = num;` we will be adding multiple wrappers. Rather than just wrapping, I think we need a `function mutRef(var)` which checks the type of the object and eg wraps a number or non-wrapper object, does nothing if it is already a wrapper object.
    // The problem with automatically wrapping mut numbers is that we can then no longer do `num === 0`. We can just *always* call .rustDeref() and add `Number.prototype.rustDeref = function() { return this; };`
    let actual = r2j_block_with_prelude!({
        let mut orig_num = 0;
        assert_eq!(orig_num, 0);

        fn add_one(num: &mut i32) -> &mut i32 {
            assert_eq!(*num, 0);
            *num += 1;
            assert_eq!(*num, 1);
            num
        }

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
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"var origNum = new RustInteger(0);
        console.assert(origNum.eq(new RustInteger(0)).jsBoolean);    
        function addOne(num) {
            console.assert(num.eq(new RustInteger(0)).jsBoolean);
            num.addAssign(new RustInteger(1));
            console.assert(num.eq(new RustInteger(1)).jsBoolean);
            return num;
        }
        {
            var result = addOne(origNum);
            console.assert(result.eq(new RustInteger(1)).jsBoolean);
            result.addAssign(new RustInteger(1));
            console.assert(result.eq(new RustInteger(2)).jsBoolean);
        }
        console.assert(origNum.eq(new RustInteger(2)).jsBoolean);
        origNum.addAssign(new RustInteger(1));
        console.assert(origNum.eq(new RustInteger(3)).jsBoolean);
        "#
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

// https://www.reddit.com/r/rust/comments/3l3fgo/what_if_rust_had_mutablebox_and_immutablebox/
// Box's primary design goal is to provide a safe interface for heap allocation. Mutability is controlled by the compiler, as you've already noticed. If you want to allow mutability only to the box's contents, you can operate through a direct reference to the contents:
// let mut my_box = Box::new(11);
// // Create an &mut i32 referencing the contents of `my_box`.
// let mut my_bof_ref = &mut *my_box;
// Edit: I neglected to mention that my_box cannot be mutated or reassigned after my_box_ref is created, since mutable references are mutually exclusive, so it creates the effect you want.
#[tokio::test]
async fn mut_ref_box_contents() {}
