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

// The rule seems to be the the enum/struct must be defined, or a use path to it must be defined, in either the same scope as the impl block (order of appearance does not matter) or in a surrounding scope, including the module top level.

// impls seem to be basically "hoisted", eg even placed in an unreachable branch, the method is still available on the original item
// fn main() {
//     struct Cool {}
//     if false {
//         fn inner() {
//             impl Cool {
//                 fn whatever(&self) {
//                     dbg!("hi");
//                 }
//             }
//         }
//     }
//     let cool = Cool {};
//     cool.whatever();
// }
// [src/main.rs:8] "hi" = "hi"

// Where different impls with the same name in different branches is considered "duplicate definitions". similarly different impls in different modules also causes a duplication error eg:
// fn main() {
//     struct Cool {}
//     if false {
//         fn inner() {
//             impl Cool {
//                 fn whatever(&self) {
//                     dbg!("hi");
//                 }
//             }
//         }
//     } else {
//         fn inner() {
//             impl Cool {
//                 fn whatever(&self) {
//                     dbg!("bye");
//                 }
//             }
//         }
//     }
//     let cool = Cool {};
//     cool.whatever();
// }
// error[E0592]: duplicate definitions with name `whatever`

fn notes() {
    // IMPORTANT TODO in Rust we can `impl Foo` with a method returning `Bar`, both defined in a lower scope than Foo, then in a higher scope, eg Foo's scope and call that method which means we have a Bar, even though it was defined in a lower scope, even though we could not directly instantiate a Bar from that scope and we might even have an identically named struct defined in the Scope. This is hard to fully recreate in JS because either we add to Foo's prototype in the scope, we can use the method, but only after the scope appears, not before like in Rust; or we hoist the impl to the scope of the target, but then the other used types from the lower scope won't be available, so they need hoisting too. This is because Rust effectively does some pretty clever hoisting, where the impl will get hoisted to the same scope as the target (surely higher if we can return it in a parent scope? NO because we need the target item to call the method on so must be in same scope or lower, well actually we can still return the instance from eg simple blocks without needing the type definition, so it can actually be used in higher scopes, just not higher fn scopes) and also "capture" any definitions it needs from the scope of the impl. This means to implement this in JS, given we need to hoist the impl def (be that adding methods to a class, adding to prototype, or having a standalone impl) to the target scope so that the methods can be used anywhere in the target impl scope, we would also need to have a duplicate Bar definition in the scope, but given there might already be another *different* Bar definition in the scope, it would need to eg be block scoped with the impl, so it is only accessible by the impl, not the rest of the scope.
    // eg:
    {
        struct Foo {}
        struct Bar {
            one: i32,
        }
        let foo = Foo {};
        let bar = foo.get_bar();
        let four = bar.two;
        {
            struct Bar {
                two: i32,
            }
            impl Foo {
                fn get_bar(&self) -> Bar {
                    Bar { two: 4 }
                }
            }
        }
    }

    // *IMPORTANT*
    // I think the solution is to hoist *all* scoped definitions, including impls, to the module level and then namespace where necessary eg `Foo_1`, `Foo_2`, etc. **BUT** this does also mean we also need to take scoped definitions into account when deduplicating module level idents...
    // The hoist to target + capture approach means we are duplicating code which is bad for bundle size
    // It also might create very hard to understand and large code becaues say we have a type with a lower impl that use some other types which are not unique, so the impl gets hoisted and the type it uses are also duplicated into the/a scope for the impl block, but one of these types also has lower impl, so we have to copy all the types *it* uses and so on.
    // In general I think we want to design for the simplest and far more common cases, however, the hoisting scoped defs to module level approach only works if *all* scoped defs are hoisted (because they of course might be using other scoped types which would need to be at the module level to be accessible).
    // Also note that scoped definitions can be made public. Whilst they can't be directly accessed/instantiated, they can be eg returned from a public fn/method, eg:
    let foo = some_mod::Foo {};
    let bar = foo.get_bar();
    assert!(bar.two == 4);
    mod some_mod {
        pub struct Foo {}

        fn some_fn() {
            pub struct Bar {
                pub two: i32,
            }
            impl Foo {
                pub fn get_bar(&self) -> Bar {
                    Bar { two: 4 }
                }
            }
        }
    }
    // So other modules can access scoped items, in which case the items also need to be hoisted to the module level from them to be usable by other modules. How can we know if an scoped item is used by another module? NO other modules can only access *instances* of scoped items, so they don't need access to the item definitions themselves.
    // Given https://github.com/rust-lang/rfcs/blob/master/text/3373-avoid-nonlocal-definitions-in-fns.md it seems that doing weird things with scoped impls is generally considered best to be avoided, so in the interest of keeping output JS more similar/familar to the original Rust and avoiding confusing naming for scoped defs hoisted to the module level, will stick with assuming scoped impl blocks are in the same scope as their target

    // This is a pretty niche and speficic case so we will leave it as a TODO but it is worth bearing in mind when considering the design.
}

#[ignore = "need to implement js class methods taking self as an arguemnt, probs using bind this"]
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
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "todo"]
#[tokio::test]
async fn full_qualified_trait_method_call() {
    setup_tracing();
    let actual = r2j_file_run_main!(
        trait Foo {
            fn get_foo(&self) -> i32;
        }
        struct Bar {}
        impl Foo for Bar {
            fn get_foo(&self) -> i32 {
                5
            }
        }
        struct Baz {}
        impl Foo for Baz {
            fn get_foo(&self) -> i32 {
                6
            }
        }

        fn main() {
            let bar = Bar {};
            dbg!(Foo::get_foo(&bar));
            let baz = Baz {};
            dbg!(Foo::get_foo(&baz));
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
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "need to move creation of rust_impl_block from handle_item_impl to extract_data_populate_item_definitions"]
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
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn inherent_impl_in_different_module() {
    setup_tracing();
    let actual = r2j_file_run_main!(
        struct Foo {}
        mod bar {
            use super::Foo;

            impl Foo {
                pub fn get_num(&self) -> i32 {
                    5
                }
            }
        }

        fn main() {
            let foo = Foo {};
            assert!(foo.get_num() == 5);
        }
    );

    let expected = format_js(
        r#"
            // crate
            class Foo {
                getNum() {
                    return 5;
                }
            }
            function main() {
                var foo = new Foo();
                console.assert(foo.getNum() === 5);
            }

            // bar

            main();
        "#,
    );
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

// #[ignore = "todo"]
#[tokio::test]
async fn scoped_inherent_impl_in_different_module() {
    setup_tracing();
    let actual = r2j_file_run_main!(
        struct Foo {}
        mod bar {
            use super::Foo;

            fn baz() {
                impl Foo {
                    pub fn get_num(&self) -> i32 {
                        5
                    }
                }
            }
        }

        fn main() {
            let foo = Foo {};
            assert!(foo.get_num() == 5);
        }
    );

    let expected = format_js(
        r#"
            // crate
            class Foo {
                getNum() {
                    return 5;
                }
            }
            function main() {
                var foo = new Foo();
                console.assert(foo.getNum() === 5);
            }

            // bar
            function baz() {}

            main();
        "#,
    );
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

// There doesn't seem any reason to implement this given that if a method is impl'd on a Struct then we expect it to be used somewhere, even if it is not accessible from certain places, so will always need it on the struct. See test private_method_in_scoped_impl below for an example.
#[ignore = "dont implement"]
#[tokio::test]
async fn dont_need_to_impl_private_method() {
    setup_tracing();
    let actual = r2j_file_run_main!(
        struct Foo {}
        mod bar {
            use super::Foo;

            fn baz() {
                impl Foo {
                    fn get_num(&self) -> i32 {
                        5
                    }
                }
            }
        }

        fn main() {
            let foo = Foo {};
            // `.get_num()` is not accessible so shouldn't appear in `class Foo {}`, even though it wouldn't cause any problems given that even private method names implented in scopes in different modules must be unique.
            // assert!(foo.get_num() == 5);
        }
    );

    let expected = format_js(
        r#"
            // crate
            class Foo {}
            function main() {
                var foo = new Foo();
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    // let _ = execute_js_with_assertions(&expected).await.unwrap();
}

// TODO move assert! to before impl to check this works or make another test for this
#[tokio::test]
async fn private_method_in_scoped_impl() {
    setup_tracing();
    let actual = r2j_file_run_main!(
        struct Foo {}
        mod bar {
            use super::Foo;

            pub fn baz() {
                let foo = Foo {};
                impl Foo {
                    fn get_num(&self) -> i32 {
                        5
                    }
                }
                assert!(foo.get_num() == 5);
            }
        }

        fn main() {
            let foo = Foo {};
            // `.get_num()` private and so not accessible here.
            // assert!(foo.get_num() == 5);
            bar::baz();
        }
    );

    let expected = format_js(
        r#"
        // crate
        class Foo {
            getNum() {
                return 5;
            }
        }
        function main() {
            var foo = new Foo();
            baz();
        }
        
        // bar
        function baz() {
            var foo = new Foo();
        
            console.assert(foo.getNum() === 5);
        }
        
        main();
        "#,
    );
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}


#[ignore = "problems transpiling class names"]
#[tokio::test]
async fn module_level_shadowing_of_struct_name() {
    setup_tracing();
    let actual = r2j_file_run_main!(
        struct Foo {}
        impl Foo {
            pub fn get_num(&self) -> i32 {
                4
            }
        }
        mod bar {
            pub struct Foo {}

            impl Foo {
                pub fn get_num(&self) -> i32 {
                    5
                }
            }
        }

        fn main() {
            let foo1 = Foo {};
            assert!(foo1.get_num() == 4);
            let foo2 = bar::Foo {};
            assert!(foo2.get_num() == 5);
        }
    );

    let expected = format_js(
        r#"
            // crate
            class Foo {
                getNum() {
                    return 4;
                }
            }

            function main() {
                var foo1 = new Foo();
                console.assert(foo1.getNum() === 4);
                var foo2 = new bar__Foo();
                console.assert(foo2.getNum() === 5);
            }

            // bar
            class bar__Foo {
                getNum() {
                    return 5;
                }
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn scoped_shadowing_of_struct_name() {
    setup_tracing();
    let actual = r2j_block_with_prelude!({
        struct Foo {}
        impl Foo {
            pub fn get_num(&self) -> i32 {
                4
            }
        }
        {
            pub struct Foo {}

            impl Foo {
                pub fn get_num(&self) -> i32 {
                    5
                }
            }
            let foo = Foo {};
            assert!(foo.get_num() == 5);
        }
        let foo = Foo {};
        assert!(foo.get_num() == 4);
    });

    // TODO need to replace var with let because let is block scoped but var is not, but then how to handle Rust variable shadowing?
    let expected = format_js(
        r#"
            class Foo {
                getNum() {
                    return 4;
                }
            }

            {
                class Foo {
                    getNum() {
                        return 5;
                    }
                }

                var foo = new Foo();
                console.assert(foo.getNum() === 5);
            }
            var foo = new Foo();
            console.assert(foo.getNum() === 4);
        "#,
    );
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
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
async fn simple_inherent_impl() {
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
async fn module_level_struct_scoped_inherent_impl() {
    setup_tracing();
    let actual = r2j_file_run_main!(
        struct Foo {
            num: i32,
        }
        fn scoped() {
            impl Foo {
                fn bar() -> i32 {
                    1
                }
                fn baz(&self) -> i32 {
                    self.num
                }
            }
        }
        fn main() {
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
            function scoped() {}
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
async fn simple_impl_trait_for_concrete() {
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

#[tokio::test]
async fn simple_scoped_impl_trait_for_type_param() {
    setup_tracing();

    let actual = r2j_block_with_prelude!({
        trait Foo {
            fn get_foo(&self) -> i32;
        }
        struct Bar {}
        impl<T> Foo for T {
            fn get_foo(&self) -> i32 {
                4
            }
        }

        let bar = Bar {};
        assert!(bar.get_foo() == 4);
    });
    let expected = format_js(
        r#"
            class Bar {
                getFoo = Foo__for__T.prototype.getFoo;
            }
            class Foo__for__T {
                getFoo() {
                    return 4;
                }
            }
            var bar = new Bar();
            console.assert(bar.getFoo() === 4);
        "#,
    );
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

// TODO `imp Foo for T` for primatives vs user types
// TODO `imp Foo for T` where self is used so need to bind this
// TODO `imp Foo for T` static vs method
// etc
#[ignore = "todo"]
#[tokio::test]
async fn multiple_scoped_impl_trait_for_type_param_for_primative() {
    setup_tracing();

    let actual = r2j_block_with_prelude!({
        trait Foo {
            fn get_foo(&self) -> i32;
        }
        trait Bar {
            fn get_bar(&self) -> i32;
        }
        impl<T> Foo for T {
            fn get_foo(&self) -> i32 {
                4
            }
        }
        impl<T> Bar for T {
            fn get_bar(&self) -> i32 {
                5
            }
        }

        let num = 1;
        num.get_foo();
        num.get_bar();
    });
    let expected = format_js(
        r#"
            class FooForT {
                getFoo() {
                    return 4;
                }
            }
            class BarForT {
                getBar() {
                    return 5;
                }
            }
            Number.prototype.getFoo = FooForT.getFoo;
            Number.prototype.getBar = BarForT.getBar;
            var num = 1;
            console.assert(num.getFoo() === 4);
            console.assert(num.getBar() === 5);
        "#,
    );
    // assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
