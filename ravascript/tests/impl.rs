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
async fn full_qualified_method_call() {
    setup_tracing();
    let actual = r2j_file_run_main!(
       struct Foo {
           num: i32,
       }
       impl Foo {
           fn baz(&self) -> i32 {
               self.num
           }
       }
       fn main () {
           let foo = Foo { num: 0 };
           assert!(Foo::baz(&foo) == 0);
       }
    );

    let expected = format_js(
        r#"
            // crate
            class Foo {
                constructor(num) {
                    this.num = num;
                }

                baz() {
                    return this.num;
                }
            }
            function main() {
                var foo = new Foo(0);
                console.assert(Foo.baz(foo) === 0);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    // let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "reason"]
#[tokio::test]
async fn call_method_before_impl_block_definition() {
    setup_tracing();
    let actual = r2j_file_run_main!(
       struct Foo {
           num: i32,
       }
       fn main () {
           let foo = Foo { num: 0 };
           assert!(foo.bar() == 0);
       }
       impl Foo {
           fn bar(&self) -> i32 {
               self.num
           }
       }
    );

    let expected = format_js(
        r#"
            // crate
            class Foo {
                constructor(num) {
                    this.num = num;
                }

                bar() {
                    return this.num;
                }
            }
            function main() {
                var foo = new Foo(0);
                console.assert(Foo.bar(foo) === 0);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    // let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "reason"]
#[tokio::test]
async fn call_method_in_same_impl_block_before_method_definition() {
    setup_tracing();
    let actual = r2j_file_run_main!(
       struct Foo {
           num: i32,
       }
       fn main () {
           let foo = Foo { num: 0 };
           assert!(foo.baz() == 0);
       }
       impl Foo {
           fn bar(&self) -> i32 {
               self.baz()
           }
           fn baz(&self) -> i32 {
               self.num
           }
       }
    );

    let expected = format_js(
        r#"
            // crate
            class Foo {
                constructor(num) {
                    this.num = num;
                }

                bar() {
                    return this.baz();
                }
                baz() {
                    return this.num;
                }
            }
            function main() {
                var foo = new Foo(0);
                console.assert(Foo.baz(foo) === 0);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    // let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn simple_method_impl() {
    setup_tracing();
    let actual = r2j_file_run_main!(
       struct Foo {
           num: i32,
       }
       impl Foo {
           fn bar() -> i32 {
               1
           }
           fn baz(&self) -> i32 {
               self.num
           }
       }
       fn main () {
           let foo = Foo { num: 0 };
           assert!(foo.baz() == 0);
           assert!(Foo::bar() == 1);
        //    assert!(Foo::baz(&foo) == 0);
       }
    );

    let expected = format_js(
        r#"
            // crate
            class Foo {
                constructor(num) {
                    this.num = num;
                }

                static bar() {
                    return 1;
                }
                baz() {
                    return this.num;
                }
            }
            function main() {
                var foo = new Foo(0);
                console.assert(foo.baz() === 0);
                console.assert(Foo.bar() === 1);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn scoped_simple_method_impl() {
    setup_tracing();
    let actual = r2j_block!({
        struct Foo {
            num: i32,
        }
        impl Foo {
            fn bar() -> i32 {
                1
            }
            fn baz(&self) -> i32 {
                self.num
            }
        }
        let foo = Foo { num: 0 };
        assert!(foo.baz() == 0);
        assert!(Foo::bar() == 1);
        // assert!(Foo::baz(&foo) == 0);
    });

    let expected = format_js(
        r#"
            class Foo {
                constructor(num) {
                    this.num = num;
                }

                static bar() {
                    return 1;
                }
                baz() {
                    return this.num;
                }
            }

            var foo = new Foo(0);
            console.assert(foo.baz() === 0);
            console.assert(Foo.bar() === 1);
        "#,
    );
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn simple_impl_trait() {
    setup_tracing();

    let actual = r2j_block_with_prelude!({
        trait Foo {
            fn get_foo(&self) -> i32;
        }
        struct Bar {
            num: i32,
        }
        impl Foo for Bar {
            fn get_foo(&self) -> i32 {
                self.num
            }
        }

        let bar = Bar { num: 5 };
        assert!(bar.get_foo() == 5);
    });
    let expected = format_js(
        r#"
            class Bar {
                constructor(num) {
                    this.num = num;
                }
            
                getFoo() {
                    return this.num;
                }
            }

            var bar = new Bar(5);
            console.assert(bar.getFoo() === 5);
        "#,
    );
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
