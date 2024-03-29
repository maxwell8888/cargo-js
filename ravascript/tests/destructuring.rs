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
    let actual = r2j_block_with_prelude!({
        struct Foo {
            bar: i32,
            baz: &'static str,
        }

        // simple destructure
        let foo = Foo { bar: 1, baz: "hi" };
        let Foo { bar, baz } = foo;
        assert_eq!(bar, 1);
        assert_eq!(baz, "hi");

        // rename destructure
        let Foo { bar: cool, .. } = foo;
        assert_eq!(cool, 1);
    });

    let expected = format_js(concat!(
        include_str!("string_prototype_extensions.js"),
        "\n",
        include_str!("number_prototype_extensions.js"),
        r#"
        class Foo {
            constructor(bar, baz) {
                this.bar = bar;
                this.baz = baz;
            }
        }
        var foo = new Foo(1, "hi");
        var { bar, baz } = foo;
        console.assert(bar.eq(1));
        console.assert(baz.eq("hi"));
        var { bar: cool } = foo;
        console.assert(cool.eq(1));
        "#,
    ));
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn destructure_struct_nested() {
    let actual = r2j_block_with_prelude!({
        struct Foo {
            bar: Bar,
        }
        struct Bar {
            baz: i32,
        }

        let foo = Foo {
            bar: Bar { baz: 1 },
        };
        let Foo { bar: Bar { baz } } = foo;
        assert_eq!(baz, 1);
    });

    let expected = concat!(
        include_str!("string_prototype_extensions.js"),
        "\n",
        include_str!("number_prototype_extensions.js"),
        r#"
        class Foo {
            constructor(bar) {
                this.bar = bar;
            }
        }
        class Bar {
            constructor(baz) {
                this.baz = baz;
            }
        }
        var foo = new Foo(new Bar(1));
        var { bar: { baz } } = foo;
        console.assert(baz.eq(1));
        "#
    );

    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn destructure_array() {
    let actual = r2j_block_with_prelude!({
        let arr = [1, 2];

        let [one, two] = arr;
        assert_eq!(one, 1);
        assert_eq!(two, 2);

        struct Foo {
            bar: i32,
        }
        let foo_arr = [Foo { bar: 1 }, Foo { bar: 2 }];
        let [Foo { bar: bar_one }, Foo { bar: bar_two }] = foo_arr;
        assert_eq!(bar_one, 1);
        assert_eq!(bar_two, 2);
    });

    let expected = format_js(concat!(
        include_str!("string_prototype_extensions.js"),
        "\n",
        include_str!("number_prototype_extensions.js"),
        r#"
        var arr = [1, 2];
        var [one, two] = arr;
        console.assert(one.eq(1));
        console.assert(two.eq(2));
        class Foo {
            constructor(bar) {
                this.bar = bar;
            }
        }
        var fooArr = [new Foo(1), new Foo(2)];
        var [{bar: barOne}, {bar: barTwo}] = fooArr;
        console.assert(barOne.eq(1));
        console.assert(barTwo.eq(2));
        "#,
    ));

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
