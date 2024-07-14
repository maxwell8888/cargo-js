mod utils;
use pretty_assertions::assert_eq;
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;
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
        let foo = new Foo(1, "hi");
        let { bar, baz } = foo;
        console.assert(bar === 1);
        console.assert(baz === "hi");
        let { bar: cool } = foo;
        console.assert(cool === 1);
        "#,
    ));
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
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
        let foo = new Foo(new Bar(1));
        let { bar: { baz } } = foo;
        console.assert(baz === 1);
        "#
    );

    assert_eq!(format_js(expected), actual);
    execute_js_with_assertions(&expected).await.unwrap();
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
        let arr = [1, 2];
        let [one, two] = arr;
        console.assert(one === 1);
        console.assert(two === 2);
        class Foo {
            constructor(bar) {
                this.bar = bar;
            }
        }
        let fooArr = [new Foo(1), new Foo(2)];
        let [{bar: barOne}, {bar: barTwo}] = fooArr;
        console.assert(barOne === 1);
        console.assert(barTwo === 2);
        "#,
    ));

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
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
    // TODO the Foo.copy() isn't needed but we add it to all structs which are marked as copy. We can remove it if it isn't needed using tracking of which methods are called but haven't implemented this yet.
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
            let foo = new Foo(1);
            let foo2 = new Foo(1);
            let arr = [foo, foo2];
            let arrRef = arr;
            let [one, _two] = arrRef.copy();
            one.num += 1;
            console.assert(one.num === 2);
            console.assert(foo.num === 1);
            foo.num += 1;
            console.assert(one.num === 2);
            console.assert(foo.num === 2);
        "#,
    ));

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "TODO implement"]
#[tokio::test]
async fn destructure_shadowing() {
    // setup_tracing();
    let actual = r2j_block_with_prelude!({
        struct Foo {
            bar: i32,
            baz: i32,
        }
        let foo = Foo { bar: 5, baz: 6 };
        #[allow(unused_variables)]
        let bar = 2;
        let Foo { bar, baz } = foo;
        assert!(bar == 5);
        assert!(baz == 6);
    });

    let expected = format_js(
        r#"
            class Foo {
                constructor(bar, baz) {
                    this.bar = bar;
                    this.baz = baz;
                }
            }
            let foo = new Foo(5, 6);
            let bar = 2;
            ({ bar, baz } = foo);
            console.assert(bar === 5);
            console.assert(baz === 6);
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "TODO implement"]
#[tokio::test]
async fn destructure_shadowing_with_block_scoping() {
    // setup_tracing();
    let actual = r2j_block_with_prelude!({
        struct Foo {
            bar: i32,
            baz: i32,
        }
        let foo = Foo { bar: 5, baz: 6 };
        let bar = 2;
        {
            let Foo { bar, baz } = foo;
            assert!(bar == 5);
            assert!(baz == 6);
        }
        assert!(bar == 2);
    });

    let expected = format_js(
        r#"
            class Foo {
                constructor(bar, baz) {
                    this.bar = bar;
                    this.baz = baz;
                }
            }
            let foo = new Foo(5, 6);
            let bar = 2;
            ({ bar, baz } = foo);
            console.assert(bar === 5);
            console.assert(baz === 6);
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}
