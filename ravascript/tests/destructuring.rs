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
async fn destructure_struct() {
    setup_tracing();
    let actual = r2j_block!({
        struct Foo {
            bar: i32,
            baz: &'static str,
        }

        // simple destructure
        let foo = Foo { bar: 1, baz: "hi" };
        let Foo { bar, baz } = foo;
        assert!(bar == 1);
        assert!(baz == "hi");

        // rename destructure
        let Foo { bar: cool, .. } = foo;
        assert!(cool == 1);
    });

    // include_str!("string_prototype_extensions.js"),
    // "\n",
    // include_str!("number_prototype_extensions.js"),
    let expected = format_js(concat!(
        r#"
        class Foo {
            constructor(bar, baz) {
                this.bar = bar;
                this.baz = baz;
            }
        }
        var foo = new Foo(1, "hi");
        var { bar, baz } = foo;
        console.assert(bar === 1);
        console.assert(baz === "hi");
        var { bar: cool } = foo;
        console.assert(cool === 1);
        "#,
    ));
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn destructure_struct_nested() {
    // setup_tracing();
    let actual = r2j_block!({
        struct Bar {
            baz: i32,
        }
        struct Foo {
            bar: Bar,
        }

        let foo = Foo {
            bar: Bar { baz: 1 },
        };
        let Foo { bar: Bar { baz } } = foo;
        assert!(baz == 1);
    });

    // include_str!("string_prototype_extensions.js"),
    // "\n",
    // include_str!("number_prototype_extensions.js"),
    let expected = concat!(
        r#"
        class Bar {
            constructor(baz) {
                this.baz = baz;
            }
        }
        class Foo {
            constructor(bar) {
                this.bar = bar;
            }
        }
        var foo = new Foo(new Bar(1));
        var { bar: { baz } } = foo;
        console.assert(baz === 1);
        "#
    );

    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn destructure_array() {
    // setup_tracing();
    let actual = r2j_block!({
        let arr = [1, 2];

        let [one, two] = arr;
        assert!(one == 1);
        assert!(two == 2);

        struct Foo {
            bar: i32,
        }
        let foo_arr = [Foo { bar: 1 }, Foo { bar: 2 }];
        let [Foo { bar: bar_one }, Foo { bar: bar_two }] = foo_arr;
        assert!(bar_one == 1);
        assert!(bar_two == 2);
    });

    // include_str!("string_prototype_extensions.js"),
    // "\n",
    // include_str!("number_prototype_extensions.js"),
    let expected = format_js(concat!(
        r#"
        var arr = [1, 2];
        var [one, two] = arr;
        console.assert(one === 1);
        console.assert(two === 2);
        class Foo {
            constructor(bar) {
                this.bar = bar;
            }
        }
        var fooArr = [new Foo(1), new Foo(2)];
        var [{bar: barOne}, {bar: barTwo}] = fooArr;
        console.assert(barOne === 1);
        console.assert(barTwo === 2);
        "#,
    ));

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn destructure_array_of_copy_structs() {
    // setup_tracing();
    let actual = r2j_block_with_prelude!({
        #[derive(Clone, Copy)]
        struct Foo {
            num: i32,
        }
        let mut foo = Foo { num: 1 };
        let foo2 = Foo { num: 1 };
        let mut arr = [foo, foo2];
        // TODO test same thing without the &mut
        let arr_ref = &mut arr;
        let [one, _two] = arr_ref;

        one.num += 1;
        assert!(one.num == 2);
        assert!(foo.num == 1);

        foo.num += 1;
        assert!(one.num == 2);
        assert!(foo.num == 2);
    });

    // include_str!("string_prototype_extensions.js"),
    // "\n",
    // include_str!("number_prototype_extensions.js"),
    
    // TODO might be better to add the `.copy()`s to the destructured vars, but would need to do this in subsequent statements which is a faff so leave for now
    let expected = format_js(concat!(
        r#"
            Array.prototype.copy = function () {
                return JSON.parse(JSON.stringify(this));
            };

            class Foo {
                constructor(num) {
                    this.num = num;
                }

                copy() {
                    return JSON.parse(JSON.stringify(this));
                }
            }
            var foo = new Foo(1);
            var foo2 = new Foo(1);
            var arr = [foo, foo];
            var arrRef = arr;
            var [one, _two] = arrRef.copy();
            one.num += 1;
            console.assert(one.num === 2);
            console.assert(foo.num === 1);
            foo.num += 1;
            console.assert(one.num === 2);
            console.assert(foo.num === 2);
        "#,
    ));

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
