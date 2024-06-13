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
    setup_tracing();
    let actual = r2j_block_with_prelude!({
        /// increments a mutable reference to an int
        fn add_one(num: &mut i32) -> &mut i32 {
            assert!(*num == 0);
            *num = 1;
            assert!(*num == 1);
            num
        }
        let mut orig_num = 0;
        assert!(orig_num == 0);
        {
            let mut result = add_one(&mut orig_num);
            assert!(*result == 1);
            // TODO what if result was a ref to a struct (copy or move)?
            let result_copy = *result;
            assert!(result_copy == 1);
            *result += 1;
            assert!(result_copy == 1);
            assert!(*result == 2);
            let six = &mut 6;
            // TODO can't use `&mut 6` after this because it is single ownership so `*result = 6` and `result = &mut 6` can be handled/parsed the same
            result = six;
            assert!(*result == 6);
        }
        assert!(orig_num == 2);
        orig_num += 1;
        assert!(orig_num == 3);
    });

    let expected = format_js(concat!(
        r#"class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }
        function addOne(num) {
            console.assert(num.inner === 0);
            num.inner = 1;
            console.assert(num.inner === 1);
            return num;
        }
        var origNum = new RustInteger(0);
        console.assert(origNum.inner === 0);
        {
            var result = addOne(origNum);
            console.assert(result.inner === 1);
            var resultCopy = result.inner;
            console.assert(resultCopy === 1);
            result.inner += 1;
            console.assert(resultCopy === 1);
            console.assert(result.inner === 2);
            var six = new RustInteger(6);
            result = six;
            console.assert(result.inner === 6);
        }
        console.assert(origNum.inner === 2);
        origNum.inner += 1;
        console.assert(origNum.inner === 3);
        "#
    ));
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn deref_vs_normal_assign() {
    let actual = r2j_block_with_prelude!({
        let mut result = &1;
        assert!(*result == 1);
        let result_copy = *result;
        assert!(result_copy == 1);
        let mut six = &6;
        // TODO can't use six after this because it is single ownership, but try similar thing with &6 to understand *result = 1 vs result = &1
        result = six;
        assert!(*result == 6);
        six = &10;
        assert!(*six == 10);
        assert!(*result == 6);
    });

    let expected = concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var result = new RustInteger(1);
        console.assert(result.inner === 1);
        var resultCopy = result.inner;
        console.assert(resultCopy === 1);
        var six = new RustInteger(6);
        result.inner = six.inner;
        console.assert(result.inner === 6);
        six.inner = 10;
        console.assert(six.inner === 10);
        console.assert(result.inner === 6);
        "#
    );

    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn ownership_mut() {
    let actual = r2j_block_with_prelude!({
        let mut foo = 5;
        fn add_one(num: &mut i32) -> &mut i32 {
            *num += 1;
            num
        }
        let bar = add_one(&mut foo);
        assert!(*bar == 6);
        *bar += 1;
        assert!(*bar == 7);
        assert!(foo == 7);

        let mut baz = 4;
        let mut baz_two = baz;
        baz_two += 1;
        assert!(baz == 4);
        assert!(baz_two == 5);

        let five = &mut 5;
        *five += 1;
        let six = five;
        assert!(*six == 6);
    });

    let expected = concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var foo = new RustInteger(5);
        function addOne(num) {
            num.inner += 1;
            return num;
        }
        var bar = addOne(foo);
        console.assert(bar.inner === 6);
        bar.inner += 1;
        console.assert(bar.inner === 7);
        console.assert(foo.inner === 7);
        var baz = new RustInteger(4);
        var bazTwo = new RustInteger(baz.inner);
        bazTwo.inner += 1;
        console.assert(baz.inner === 4);
        console.assert(bazTwo.inner === 5);
        var five = new RustInteger(5);
        five.inner += 1;
        var six = five;
        console.assert(six.inner === 6);
        "#
    );

    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn mutate_int() {
    let actual = r2j_block_with_prelude!({
        let mut num = 0;
        num += 1;
        assert!(num == 1);
    });

    let expected = concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var num = new RustInteger(0);
        num.inner += 1;
        console.assert(num.inner === 1)"#
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn copy_and_mutate() {
    let actual = r2j_block_with_prelude!({
        let mut orig_num = 0;
        let copy_num = orig_num;
        orig_num += 1;
        // NOTE assert_eq! doesn't need a .copy() because it doesn't mutate it's args and immediately releases them so basically has no effect
        assert!(copy_num == 0);
        assert!(orig_num == 1);
    });

    let expected = format_js(concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var origNum = new RustInteger(0);
        var copyNum = origNum.inner;
        origNum.inner += 1;
        console.assert(copyNum === 0);
        console.assert(origNum.inner === 1);
        "#
    ));
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn copy_mut_ref() {
    let actual = r2j_block_with_prelude!({
        let mut_ref = &mut 0;
        let copy_mut_ref = *mut_ref;
        *mut_ref += 1;
        assert!(*mut_ref == 1);
        assert!(copy_mut_ref == 0);
    });

    let expected = format_js(concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var mutRef = new RustInteger(0);
        var copyMutRef = mutRef.inner;
        mutRef.inner += 1;
        console.assert(mutRef.inner === 1);
        console.assert(copyMutRef === 0);
        "#
    ));
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn non_mut_copy_to_mut() {
    let actual = r2j_block_with_prelude!({
        let orig_num = 0;
        let mut copy_num = orig_num;
        copy_num += 1;
        assert!(copy_num == 1);
        assert!(orig_num == 0);
    });

    let expected = format_js(concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var origNum = 0;
        var copyNum = new RustInteger(origNum);
        copyNum.inner += 1;
        console.assert(copyNum.inner === 1);
        console.assert(origNum === 0);
        "#
    ));
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn mut_ref_from_non_mut() {
    let actual = r2j_block_with_prelude!({
        let mut orig_num = 0;
        let mut_ref_num = &mut orig_num;
        *mut_ref_num += 1;
        assert!(*mut_ref_num == 1);
        assert!(orig_num == 1);
    });

    let expected = format_js(concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var origNum = new RustInteger(0);
        var mutRefNum = origNum;
        mutRefNum.inner += 1;
        console.assert(mutRefNum.inner === 1);
        console.assert(origNum.inner === 1);
        "#
    ));
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn mutate_int_fn_arg() {
    let actual = r2j_block_with_prelude!({
        let orig_num = 0;
        fn add_one(mut num: i32) -> i32 {
            num += 2;
            assert!(num == 2);
            num += num;
            assert!(num == 4);
            num
        }
        let result = add_one(orig_num);
        assert!(result == 4);
        assert!(orig_num == 0);
    });

    let expected = format_js(concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var origNum = 0;
        function addOne(num) {
            var num = new RustInteger(num);
            num.inner += 2;
            console.assert(num.inner === 2);
            num.inner += num.inner;
            console.assert(num.inner === 4);
            return num.inner;
        }
        var result = addOne(origNum);
        console.assert(result === 4);
        console.assert(origNum === 0);
        "#
    ));
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
        assert!(orig_num == 0);
        fn add_one(num: &mut i32) -> &mut i32 {
            assert!(*num == 0);
            *num += 1;
            assert!(*num == 1);
            num
        }
        {
            let result = add_one(&mut orig_num);
            assert!(*result == 1);
            *result += 1;
            assert!(*result == 2);
        }
        assert!(orig_num == 2);
        orig_num += 1;
        assert!(orig_num == 3);
    });

    let expected = concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var origNum = new RustInteger(0);
        console.assert(origNum.inner === 0);    
        function addOne(num) {
            console.assert(num.inner === 0);
            num.inner += 1;
            console.assert(num.inner === 1);
            return num;
        }
        {
            var result = addOne(origNum);
            console.assert(result.inner === 1);
            result.inner += 1;
            console.assert(result.inner === 2);
        }
        console.assert(origNum.inner === 2);
        origNum.inner += 1;
        console.assert(origNum.inner === 3);
        "#
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn copy_mut_inside_fn() {
    let actual = r2j_block_with_prelude!({
        let orig_num = 0;
        fn add_one(mut num: i32) -> i32 {
            let other = num;
            num += 1;
            assert!(num == 1);
            assert!(other == 0);
            num
        }
        let result = add_one(orig_num);
        assert!(result == 1);
        assert!(orig_num == 0);
    });

    let expected = format_js(concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var origNum = 0;
        function addOne(num) {
            var num = new RustInteger(num);
            var other = num.inner;
            num.inner += 1;
            console.assert(num.inner === 1);
            console.assert(other === 0);
            return num.inner;
        }
        var result = addOne(origNum);
        console.assert(result === 1);
        console.assert(origNum === 0);
        "#
    ));
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn mut_ref_to_copy() {
    let actual = r2j_block_with_prelude!({
        let mut orig_num = 0;

        fn add_one(num: &mut i32) -> i32 {
            assert!(*num == 0);
            *num += 1;
            assert!(*num == 1);
            *num
        }

        let mut result = add_one(&mut orig_num);
        assert!(result == 1);
        assert!(orig_num == 1);

        result += 1;
        assert!(result == 2);
        assert!(orig_num == 1);

        orig_num += 1;
        assert!(result == 2);
        assert!(orig_num == 2);
    });

    let expected = format_js(concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var origNum = new RustInteger(0);
        function addOne(num) {
            console.assert(num.inner === 0);
            num.inner += 1;
            console.assert(num.inner === 1);
            return num.inner;
        }
        var result = new RustInteger(addOne(origNum));
        console.assert(result.inner === 1);
        console.assert(origNum.inner === 1);
        result.inner += 1;
        console.assert(result.inner === 2);
        console.assert(origNum.inner === 1);
        origNum.inner += 1;
        console.assert(result.inner === 2);
        console.assert(origNum.inner === 2);
        "#
    ));
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn fn_call_return_mut_ref_to_copy() {
    let actual = r2j_block_with_prelude!({
        let mut orig_num = 0;

        fn mut_ref_do_nothing(num: &mut i32) -> &mut i32 {
            num
        }

        fn add_one(num: &mut i32) -> i32 {
            assert!(*num == 0);
            *num += 1;
            assert!(*num == 1);
            *mut_ref_do_nothing(num)
        }

        let mut result = add_one(&mut orig_num);
        assert!(result == 1);
        assert!(orig_num == 1);

        result += 1;
        assert!(result == 2);
        assert!(orig_num == 1);

        orig_num += 1;
        assert!(result == 2);
        assert!(orig_num == 2);
    });

    let expected = format_js(concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var origNum = new RustInteger(0);
        function mutRefDoNothing(num) {
            return num;
        }
        function addOne(num) {
            console.assert(num.inner === 0);
            num.inner += 1;
            console.assert(num.inner === 1);
            return mutRefDoNothing(num).inner;
        }
        var result = new RustInteger(addOne(origNum));
        console.assert(result.inner === 1);
        console.assert(origNum.inner === 1);
        result.inner += 1;
        console.assert(result.inner === 2);
        console.assert(origNum.inner === 1);
        origNum.inner += 1;
        console.assert(result.inner === 2);
        console.assert(origNum.inner === 2);
        "#
    ));
    // println!("{expected}");
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn mutating_non_copy_value() {
    let actual = r2j_block_with_prelude!({
        let mut orig_num = 0;
        fn add_one(num: &mut i32) -> i32 {
            assert!(*num == 0);
            *num += 1;
            assert!(*num == 1);
            *num
        }
        {
            let mut result = add_one(&mut orig_num);
            assert!(result == 1);
            result += 1;
            assert!(result == 2);
        }
        assert!(orig_num == 1);
        orig_num += 1;
        assert!(orig_num == 2);
    });

    let expected = concat!(
        "class RustInteger {
            constructor(inner) {
                this.inner = inner;
            }
        }",
        r#"var origNum = new RustInteger(0);
        function addOne(num) {
            console.assert(num.inner === 0);
            num.inner += 1;
            console.assert(num.inner === 1);
            return num.inner;
        }
        {
            var result = new RustInteger(addOne(origNum));
            console.assert(result.inner === 1);
            result.inner += 1;
            console.assert(result.inner === 2);
        }
        console.assert(origNum.inner === 1);
        origNum.inner += 1;
        console.assert(origNum.inner === 2);
        "#
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn pass_mut_var_as_imm_ref() {
    let actual = r2j_block_with_prelude!({
        let mut five = 5;
        five += 1;
        fn foo(num: &i32) -> i32 {
            num + 2
        }
        let result = foo(&five);
        five += 1;
        assert!(five == 7);
        assert!(result == 8);
    });

    let expected = format_js(
        r#"
            class RustInteger {
                constructor(inner) {
                    this.inner = inner;
                }
            }
            var five = new RustInteger(5);
            five.inner += 1;
            function foo(num) {
                return num + 2;
            }
            var result = foo(five.inner);
            five.inner += 1;
            console.assert(five.inner === 7);
            console.assert(result === 8);
        "#,
    );

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn get_box_contents() {
    let actual = r2j_block_with_prelude!({
        let box_num = Box::new(1);
        {
            let mut num_ref = *box_num;
            num_ref += 1;
            assert!(num_ref == 2);
        }
        assert!(*box_num == 1);
    });

    let expected = format_js(
        r#"
            class RustInteger {
                constructor(inner) {
                    this.inner = inner;
                }
            }
            var boxNum = 1;
            {
                var numRef = new RustInteger(boxNum);
                numRef.inner += 1;
                console.assert(numRef.inner === 2);
            }
            console.assert(boxNum === 1);
        "#,
    );

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn get_box_contents_with_mut() {
    let actual = r2j_block_with_prelude!({
        let mut box_num = Box::new(1);
        {
            let mut num_ref = *box_num;
            num_ref += 1;
            assert!(num_ref == 2);
            *box_num += 5;
            assert!(num_ref == 2);
            assert!(*box_num == 6);
        }
        *box_num += 5;
        assert!(*box_num == 11);
        box_num = Box::new(2);
        assert!(*box_num == 2);
    });

    let expected = format_js(
        r#"
            class RustInteger {
                constructor(inner) {
                    this.inner = inner;
                }
            }
            var boxNum = new RustInteger(1);
            {
                var numRef = new RustInteger(boxNum.inner);
                numRef.inner += 1;
                console.assert(numRef.inner === 2);
                boxNum.inner += 5;
                console.assert(numRef.inner === 2);
                console.assert(boxNum.inner === 6);
            }
            boxNum.inner += 5;
            console.assert(boxNum.inner === 11);
            boxNum = 2;
            console.assert(boxNum === 2);
        "#,
    );

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

// https://www.reddit.com/r/rust/comments/3l3fgo/what_if_rust_had_mutablebox_and_immutablebox/
// Box's primary design goal is to provide a safe interface for heap allocation. Mutability is controlled by the compiler, as you've already noticed. If you want to allow mutability only to the box's contents, you can operate through a direct reference to the contents:
// let mut my_box = Box::new(11);
// // Create an &mut i32 referencing the contents of `my_box`.
// let mut my_bof_ref = &mut *my_box;
// Edit: I neglected to mention that my_box cannot be mutated or reassigned after my_box_ref is created, since mutable references are mutually exclusive, so it creates the effect you want.
#[tokio::test]
async fn mut_ref_box_contents() {
    let actual = r2j_block_with_prelude!({
        let mut box_num = Box::new(1);
        {
            let num_ref = &mut *box_num;
            *num_ref += 1;
            assert!(*num_ref == 2);
        }
        assert!(*box_num == 2);
    });

    let expected = format_js(
        r#"
            class RustInteger {
                constructor(inner) {
                    this.inner = inner;
                }
            }
            var boxNum = new RustInteger(1);
            {
                var numRef = boxNum;
                numRef.inner += 1;
                console.assert(numRef.inner === 2);
            }
            console.assert(boxNum.inner === 2);
        "#,
    );

    // assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
