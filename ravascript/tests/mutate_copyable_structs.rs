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
async fn mut_ref_field() {
    let actual = r2j_block_with_prelude!({
        struct Foo<'a> {
            num: &'a mut i32,
        }
        let mut five = 5;
        let foo = Foo { num: &mut five };
        *foo.num += 1;
        assert!(*foo.num == 6);
        five += 1;
        assert!(five == 7);
    });

    let expected = format_js(
        r#"
            class RustInteger {
                constructor(inner) {
                    this.inner = inner;
                }
            }
            class Foo {
                constructor(num) {
                    this.num = num;
                }
            }
            var five = new RustInteger(5);
            var foo = new Foo(five);
            foo.num.inner += 1;
            console.assert(foo.num.inner === 6);
            five.inner += 1;
            console.assert(five.inner === 7);
        "#,
    );

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn reassign_mut_ref_field() {
    let actual = r2j_block_with_prelude!({
        struct Foo<'a> {
            num: &'a mut i32,
        }
        let mut five = 5;
        let mut foo = Foo { num: &mut five };
        let mut_ref_two = &mut 2;
        {
            foo.num = mut_ref_two;
            assert!(*foo.num == 2);
            *foo.num += 1;
            assert!(*foo.num == 3);
            assert!(*mut_ref_two == 3);
        }
        *mut_ref_two += 1;
        assert!(*mut_ref_two == 4);
    });

    let expected = format_js(
        r#"
            class RustInteger {
                constructor(inner) {
                    this.inner = inner;
                }
            }
            class Foo {
                constructor(num) {
                    this.num = num;
                }
            }
            var five = new RustInteger(5);
            var foo = new Foo(five);
            var mutRefTwo = new RustInteger(2);
            {
                foo.num = mutRefTwo;
                console.assert(foo.num.inner === 2);
                foo.num.inner += 1;
                console.assert(foo.num.inner === 3);
                console.assert(mutRefTwo.inner === 3);
            }
            mutRefTwo.inner += 1;
            console.assert(mutRefTwo.inner === 4);
        "#,
    );

    assert_eq!(expected, actual);
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

    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
