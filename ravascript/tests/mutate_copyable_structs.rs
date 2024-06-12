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
        five += 1;
        assert!(*mut_ref_two == 4);
        assert!(five == 6);
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
            five.inner += 1;
            console.assert(mutRefTwo.inner === 4);
            console.assert(five.inner === 6);
        "#,
    );

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn copy_struct_with_ref_field() {
    let actual = r2j_block_with_prelude!({
        #[derive(Clone, Copy)]
        struct Foo<'a> {
            num: &'a i32,
        }
        let mut five = 5;
        let mut foo = Foo { num: &mut five };
        let mut_ref_two = &mut 2;
        {
            foo.num = mut_ref_two;
            assert!(*foo.num == 2);
            assert!(*mut_ref_two == 2);
        }
        *mut_ref_two += 1;
        five += 1;
        assert!(*mut_ref_two == 3);
        assert!(five == 6);
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

                copy() {
                    return JSON.parse(JSON.stringify(this));
                }
            }
            var five = new RustInteger(5);
            var foo = new Foo(five.inner);
            var mutRefTwo = new RustInteger(2);
            {
                foo.num = mutRefTwo.inner;
                console.assert(foo.num === 2);
                console.assert(mutRefTwo.inner === 2);
            }
            mutRefTwo.inner += 1;
            five.inner += 1;
            console.assert(mutRefTwo.inner === 3);
            console.assert(five.inner === 6);
        "#,
    );

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "reason"]
#[tokio::test]
async fn pass_mut_var_as_imm_ref() {
    let actual = r2j_block_with_prelude!({
        let mut five = 5;
        five += 1;
        fn foo(num: &i32) -> i32 {
            num + 2
        }
        let result = foo(&five);
        assert!(result == 8)
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

                copy() {
                    return JSON.parse(JSON.stringify(this));
                }
            }
            var five = new RustInteger(5);
            var foo = new Foo(five.inner);
            var mutRefTwo = new RustInteger(2);
            {
                foo.num = mutRefTwo.inner;
                console.assert(foo.num === 2);
                console.assert(mutRefTwo.inner === 2);
            }
            mutRefTwo.inner += 1;
            five.inner += 1;
            console.assert(mutRefTwo.inner === 3);
            console.assert(five.inner === 6);
        "#,
    );

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
