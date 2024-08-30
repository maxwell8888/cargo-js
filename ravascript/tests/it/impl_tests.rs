use pretty_assertions::assert_eq;
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;

use super::utils::*;
use crate::{r2j_block, r2j_block_with_prelude, r2j_file_run_main};

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

#[allow(unused_variables, dead_code)]
fn _notes() {
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

#[ignore = "LOW PRIORITY"]
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

    // NOTE we simply translate back to a normal method call since a method must take self as the first argument therefore we must have an instance and can therefore just call the method.
    // AFAICT the only alternative in JS is something like `const baz = foo.baz.bind(foo);` which seems redundant.
    let expected = format_js(
        r#"
            class Foo {
                constructor(num) {
                    this.num = num;
                }

                baz() {
                    return this.num;
                }
            }
            
            function main() {
                let foo = new Foo(0);
                console.assert(Foo.baz(foo) === 0);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "LOW PRIORITY"]
#[tokio::test]
async fn full_qualified_trait_method_call() {
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
            class Foo {
                constructor(num) {
                    this.num = num;
                }

                baz() {
                    return this.num;
                }
            }

            function main() {
                let foo = new Foo(0);
                console.assert(Foo.baz(foo) === 0);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn call_method_before_impl_block_definition() {
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
            class Foo {
                constructor(num) {
                    this.num = num;
                }

                bar() {
                    return this.num;
                }
            }
            function main() {
                let foo = new Foo(0);
                console.assert(foo.bar() === 0);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn multiple_impls_with_same_signature() {
    setup_tracing();
    let actual = r2j_file_run_main!(
        struct Foo {}
        impl Foo {
            fn bar(&self) -> i32 {
                4
            }
        }
        impl Foo {
            fn baz(&self) -> i32 {
                5
            }
        }

        fn main() {
            let foo = Foo {};
            assert!(foo.bar() == 4);
            assert!(foo.baz() == 5);
        }
    );

    let expected = format_js(
        r#"
            class Foo {
                bar() {
                    return 4;
                }
                baz() {
                    return 5;
                }
            }

            function main() {
                let foo = new Foo();
                console.assert(foo.bar() === 4);
                console.assert(foo.baz() === 5);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
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
                let foo = new Foo();
                console.assert(foo.getNum() === 5);
            }

            // bar

            main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[allow(dead_code)]
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
                let foo = new Foo();
                console.assert(foo.getNum() === 5);
            }

            // bar
            function baz() {}

            main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// In the majority of cases, we could simply store all impl blocks globally, and apply them to all classes. However, there are some cases which mean we need to record the scope of the impl (assuming it is scoped) to properly apply it to the correct struct. The intention of this test is to demonstrate this case.
#[tokio::test]
async fn shadowed_structs_with_shadowed_methods() {
    setup_tracing();
    // let actual = r2j_block_unformatted!({
    let actual = r2j_file_run_main!(
        fn main() {
            struct Foo {}
            impl Foo {
                fn bar(&self) -> i32 {
                    4
                }
            }
            fn cool() {
                struct Foo {}
                impl Foo {
                    fn bar(&self) -> i32 {
                        5
                    }
                }
                let foo = Foo {};
                assert!(foo.bar() == 5);
            }
            let foo = Foo {};
            assert!(foo.bar() == 4);
            cool();
        }
    );
    // });
    // println!("{actual}");

    let expected = format_js(
        r#"
            function main() {
                class Foo {
                    bar() {
                        return 4;
                    }
                }

                function cool() {
                    class Foo {
                        bar() {
                            return 5;
                        }
                    }
                    
                    let foo = new Foo();
                    console.assert(foo.bar() === 5);
                }
                let foo = new Foo();
                console.assert(foo.bar() === 4);
                cool();
            }
            
            main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// There doesn't seem any reason to implement this given that if a method is impl'd on a Struct then we expect it to be used somewhere, even if it is not accessible from certain places, so will always need it on the struct. See test private_method_in_scoped_impl below for an example.
#[allow(dead_code, unused_variables)]
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
                let foo = new Foo();
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    // execute_js_with_assertions(&expected).await.unwrap();
}

// TODO move assert! to before impl to check this works or make another test for this
#[allow(unused_variables)]
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
            let foo = new Foo();
            baz();
        }
        
        // bar
        function baz() {
            let foo = new Foo();
        
            console.assert(foo.getNum() === 5);
        }
        
        main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

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
            class crate__Foo {
                getNum() {
                    return 4;
                }
            }

            function main() {
                let foo1 = new crate__Foo();
                console.assert(foo1.getNum() === 4);
                let foo2 = new bar__Foo();
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
    execute_js_with_assertions(&expected).await.unwrap();
}

#[allow(dead_code)]
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
        fn another_scope() {
            pub struct Foo {}

            impl Foo {
                pub fn get_num(&self) -> i32 {
                    5
                }
            }
            let foo = Foo {};
            assert!(foo.get_num() == 5);
        }
        another_scope();
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

            function anotherScope() {
                class Foo {
                    getNum() {
                        return 5;
                    }
                }

                let foo = new Foo();
                console.assert(foo.getNum() === 5);
            }
            anotherScope();
            let foo = new Foo();
            console.assert(foo.getNum() === 4);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// Need to be able to lookup return type of other method call, before we have finished processing the impl block, this is a problem no matter where we do the processing, once we want to process the body to get the JS and return type, we also need to do the same for the other method, but it might appear afterwards. I think the only solutions are:
// 1. To dynamically work out the order in which methods/fns/etc need to be parsed (they can't both call each other since that would cause an infinite loop, so there is in fact an order they should be processed in)
// 2. To use the fn signature to determine the return type
#[tokio::test]
async fn call_method_in_same_impl_block_before_method_definition() {
    setup_tracing();
    let actual = r2j_file_run_main!(
       struct Foo {
           num: i32,
       }
       fn main () {
           let foo = Foo { num: 0 };
           assert!(foo.bar() == 0);
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
                let foo = new Foo(0);
                console.assert(foo.bar() === 0);
                console.assert(foo.baz() === 0);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
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
                let foo = new Foo(0);
                console.assert(foo.baz() === 0);
                console.assert(Foo.bar() === 1);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[allow(dead_code)]
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
                let foo = new Foo(0);
                console.assert(foo.baz() === 0);
                console.assert(Foo.bar() === 1);
            }

            main();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[allow(dead_code)]
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

            let foo = new Foo(0);
            console.assert(foo.baz() === 0);
            console.assert(Foo.bar() === 1);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
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

            let bar = new Bar(5);
            console.assert(bar.getFoo() === 5);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn impl_trait_for_number() {
    setup_tracing();

    let actual = r2j_block_with_prelude!({
        trait Foo {
            fn plus_one(&self) -> i32;
        }
        impl Foo for i32 {
            fn plus_one(&self) -> i32 {
                self + 1
            }
        }

        let one = 1;
        let two = one.plus_one();
        assert!(one == 1);
        assert!(two == 2);
        let four = 3.plus_one();
        assert!(four == 4);
    });
    let expected = format_js(
        r#"
            class Foo__i32 {
                plusOne() {
                    return this + 1;
                }
            }
            Number.prototype.plusOne = Foo__i32.prototype.plusOne;
            let one = 1;
            let two = one.plusOne();
            console.assert(one === 1);
            console.assert(two === 2);
            let four = (3).plusOne();
            console.assert(four === 4);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
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
            Array.prototype.getFoo = Foo__for__T.prototype.getFoo;
            Boolean.prototype.getFoo = Foo__for__T.prototype.getFoo;
            Number.prototype.getFoo = Foo__for__T.prototype.getFoo;
            String.prototype.getFoo = Foo__for__T.prototype.getFoo;
            let bar = new Bar();
            console.assert(bar.getFoo() === 4);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// TODO `imp Foo for T` static vs method
// etc
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
        assert!(num.get_foo() == 4);
        assert!(num.get_bar() == 5);
    });

    // TODO should ideally not add/remove the `String.prototype...`s since they aren't actually used
    let expected = format_js(
        r#"
            class Foo__for__T {
                getFoo() {
                    return 4;
                }
            }
            Array.prototype.getFoo = Foo__for__T.prototype.getFoo;
            Boolean.prototype.getFoo = Foo__for__T.prototype.getFoo;
            Number.prototype.getFoo = Foo__for__T.prototype.getFoo;
            String.prototype.getFoo = Foo__for__T.prototype.getFoo;
            class Bar__for__T {
                getBar() {
                    return 5;
                }
            }
            Array.prototype.getBar = Bar__for__T.prototype.getBar;
            Boolean.prototype.getBar = Bar__for__T.prototype.getBar;
            Number.prototype.getBar = Bar__for__T.prototype.getBar;
            String.prototype.getBar = Bar__for__T.prototype.getBar;
            let num = 1;
            console.assert(num.getFoo() === 4);
            console.assert(num.getBar() === 5);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn trait_generic_impl() {
    setup_tracing();

    let actual = r2j_block_with_prelude!({
        struct Foo {
            num: i32,
        }
        trait Bar {
            fn get_num(&self) -> i32;
        }
        impl<T> Bar for T {
            fn get_num(&self) -> i32 {
                5
            }
        }

        let bar = Foo { num: 1 };
        assert!(bar.get_num() == 5);
        let num = 1;
        assert!(num.get_num() == 5);
        let text = "hello";
        assert!(text.get_num() == 5);
    });
    let expected = format_js(
        r#"
            class Foo {
                constructor(num) {
                    this.num = num;
                }
                getNum = Bar__for__T.prototype.getNum;
            }

            class Bar__for__T {
                getNum() {
                    return 5;
                }
            }
            Array.prototype.getNum = Bar__for__T.prototype.getNum;
            Boolean.prototype.getNum = Bar__for__T.prototype.getNum;
            Number.prototype.getNum = Bar__for__T.prototype.getNum;
            String.prototype.getNum = Bar__for__T.prototype.getNum;
            let bar = new Foo(1);
            console.assert(bar.getNum() === 5);
            let num = 1;
            console.assert(num.getNum() === 5);
            let text = "hello";
            console.assert(text.getNum() === 5);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn trait_default_impl() {
    setup_tracing();

    let actual = r2j_block_with_prelude!({
        struct Foo {
            num: i32,
        }
        trait Bar {
            fn get_five(&self) -> i32 {
                5
            }
        }
        impl Bar for Foo {}

        let bar = Foo { num: 1 };
        assert!(bar.get_five() == 5);
    });
    let expected = format_js(
        r#"
            class Foo {
                constructor(num) {
                    this.num = num;
                }
                getFive = Bar.prototype.getFive;
            }
            class Bar {
                getFive() {
                    return 5;
                }
            }

            let bar = new Foo(1);
            console.assert(bar.getFive() === 5);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// #[tokio::test]
// async fn super_trait_default_impl() {
//     setup_tracing();

//     let actual = r2j_block_with_prelude!({
//         struct Foo {
//             num: i32,
//         }
//         trait Bar {
//             fn get_num(&self) -> i32 {
//                 4
//             }
//         }
//         trait Baz: Bar {}
//         impl Bar for Foo {}
//         impl Baz for Foo {}

//         let bar = Foo { num: 5 };
//         assert!(bar.get_foo() == 5);
//     });
//     let expected = format_js(
//         r#"
//             class Bar {
//                 constructor(num) {
//                     this.num = num;
//                 }

//                 getFoo() {
//                     return this.num;
//                 }
//             }

//             let bar = new Bar(5);
//             console.assert(bar.getFoo() === 5);
//         "#,
//     );
//     assert_eq!(expected, actual);
//     execute_js_with_assertions(&expected).await.unwrap();
// }

#[allow(dead_code, unused_mut)]
#[tokio::test]
async fn mut_method_call_struct() {
    let actual = r2j_block_with_prelude!({
        struct Foo {
            num: i32,
        }
        impl Foo {
            fn plus_one(&self) -> i32 {
                self.num + 1
            }
        }
        let mut foo = Foo { num: 1 };
        let result = foo.plus_one();
        let mut mut_result = foo.plus_one();
        mut_result += 1;
        assert!(result == 2);
        assert!(mut_result == 3);
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

                plusOne() {
                    return this.num + 1;
                }
            }

            let foo = new Foo(1);
            let result = foo.plusOne();
            let mutResult = new RustInteger(foo.plusOne());
            mutResult.inner += 1;
            console.assert(result === 2);
            console.assert(mutResult.inner === 3);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[allow(unused_mut)]
#[tokio::test]
async fn mut_method_call_num() {
    let actual = r2j_block_with_prelude!({
        trait PlusOne {
            fn plus_one(&self) -> i32;
        }
        impl PlusOne for i32 {
            fn plus_one(&self) -> i32 {
                self + 1
            }
        }
        let mut num = 1;
        let result = num.plus_one();
        let mut mut_result = num.plus_one();
        mut_result += 1;
        assert!(result == 2);
        assert!(mut_result == 3);
    });

    let expected = format_js(
        r#"
            class RustInteger {
                constructor(inner) {
                    this.inner = inner;
                }
            }

            class PlusOne__i32 {
                plusOne() {
                    return this + 1;
                }
            }
            Number.prototype.plusOne = PlusOne__i32.prototype.plusOne;
            let num = new RustInteger(1);
            let result = num.inner.plusOne();
            let mutResult = new RustInteger(num.inner.plusOne());
            mutResult.inner += 1;
            console.assert(result === 2);
            console.assert(mutResult.inner === 3);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// #[ignore = "reason"]
#[allow(unused_mut)]
#[tokio::test]
async fn method_call_num_mut_self() {
    let actual = r2j_block_with_prelude!({
        trait IncOne {
            fn inc_one(&mut self);
        }
        impl IncOne for i32 {
            fn inc_one(&mut self) {
                *self += 1;
            }
        }
        let mut num = 1;
        num.inc_one();
        assert!(num == 2);
    });

    let expected = format_js(
        r#"
            class RustInteger {
                constructor(inner) {
                    this.inner = inner;
                }
            }

            class IncOne__i32 {
                incOne() {
                    this.inner += 1;
                }
            }
            RustInteger.prototype.incOne = IncOne__i32.prototype.incOne;
            let num = new RustInteger(1);
            num.incOne();
            console.assert(num.inner === 2);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// Currently we aren't storing generic bounds on `RustTypeParam`s. However we will have to in the case that eg a method is called on a type param, in order to be able to look up the appropriate trait.
#[ignore = "TODO MED PRIORITY"]
#[allow(unused_mut)]
#[tokio::test]
async fn method_call_on_type_param() {
    let actual = r2j_block_with_prelude!({
        trait Foo {
            fn get_num(&self) -> i32 {
                4
            }
        }
        fn bar<T: Foo>(foo: T) {
            foo.get_num();
        }
    });

    let expected = format_js(
        r#"
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// TODO support trait bounds where the trait is generic
#[ignore = "TODO MED PRIORITY"]
#[allow(unused_mut)]
#[tokio::test]
async fn method_call_on_type_param_generic_trait_bounds() {
    let actual = r2j_block_with_prelude!({
        trait Foo<T> {
            fn get_num(&self) -> i32 {
                4
            }
        }
        fn bar<T, U: Foo<T>>(foo: U) {
            foo.get_num();
        }
    });

    let expected = format_js(
        r#"
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}
