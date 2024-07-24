mod utils;
use pretty_assertions::assert_eq;
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;
use utils::*;

#[tokio::test]
async fn testing_asserts() -> Result<(), Box<dyn std::error::Error>> {
    let js_should_not_throw = "console.assert(3 === 3, { msg: 'numbers do not match' });";
    execute_js_with_assertions(js_should_not_throw).await?;

    let js_should_throw = "console.assert(3 === 2, { msg: 'numbers do not match' });";
    assert!(execute_js_with_assertions(js_should_throw).await.is_err());
    Ok(())
}

#[tokio::test]
async fn it_executes_simple_expressions() {
    let generated_js = r2j_block_with_prelude!({
        let my_num = 5;
        assert!(my_num == 2 + 3);
        enum Colors {
            #[allow(dead_code)]
            Red,
            Blue,
        }
        let blue = Colors::Blue;
        let answer = match blue {
            Colors::Red => 1,
            Colors::Blue => 2,
        };
        assert!(answer == 2);
    });

    execute_js_with_assertions(&generated_js).await.unwrap();
}

#[allow(clippy::useless_vec)]
#[tokio::test]
async fn it_transpiles_vec_macro() {
    let actual = r2j_block!({
        let _data = vec![1, 2, 3];
    });
    assert_eq!("let _data = [1, 2, 3];", actual);
}

#[allow(clippy::useless_vec)]
#[tokio::test]
async fn it_transpiles_vec_macro2() {
    let actual = r2j_block_with_prelude!({
        let data = vec![1, 2, 3];
        assert!(data[1] == 2);
    });

    let expected = format_js(
        r#"
            let data = [1, 2, 3];
            console.assert(data[1] === 2)
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[allow(clippy::useless_vec)]
#[tokio::test]
async fn it_transpiles_iter_map() {
    let actual = r2j_block!({
        let data = vec![1, 2, 3];
        let _data = data
            .iter()
            .map(|num: &i32| {
                let _sum = num + 2;
                num
            })
            .collect::<Vec<_>>();
    });
    let expected = format_js(
        r#"
            let data = [1, 2, 3];
            let _data = data.map((num) => {
                let _sum = num + 2;
                return num;
            });
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn if_expr_var() {
    let actual = r2j_block!({
        let num = if true { 1 } else { 0 };
        assert!(num == 1);
    });
    let expected = format_js(
        r#"
        var num;
        if (true) {
            num = 1;
        } else {
            num = 0;
        }
        console.assert(num === 1);
        "#,
    );
    assert_eq!(expected, actual);
}

// TODO our current approach to if expressions doesn't work if they eg need to be passed as an argument or wrapped in RustInteger like below. Switch to using either:
// 1. ternary expressions, which can't have multiple stmts, so need to wrap sections which >1 stmts in an iffe
// 2. Use a temp var and wrap the whole expression in an iffe like below
// I don't think one is better than the other, teraries will be horrible with many sections with iffes, but much cleaner if single stmts, also ternary if_else are hard to read, so 2 is probs a better deafult initially until we can impl both, choosing the best approach per case.
#[ignore = "TODO"]
#[tokio::test]
async fn if_expr_mut_var() {
    let actual = r2j_block!({
        let mut num = if true { 1 } else { 0 };
        num += 1;
        assert!(num == 2);
    });
    let expected = format_js(
        r#"
            let num = new RustInteger(
                (() => {
                    if (true) {
                        return 1;
                    } else {
                        return 0;
                    }
                })()
            )
            console.assert(num === 1);
        "#,
    );
    assert_eq!(expected, actual);
}

// TODO should actually implement these because even though they are not real world examples, they force all the Expr handling to use the right abstractions and patterns
#[ignore = "low priority"]
#[tokio::test]
async fn reassign_box_new() {
    let actual = r2j_block!({
        // To assign to box_new, we need to lookup Box::new. In looking it up we find that it is Box::new and rather than assign a normal RustType::Fn, we assign eg a RustType::FnVanish, this way when box_new gets called we can know that the call should vanish and just replace it with the arg. I think we can just vanish the whole `let box_new = Box::<i32>::new;` line too?? (But still create the ScopedVar to hold the name and RustType::FnVanish) What if we are doing something more complex with it like passing it to a fn?
        let box_new = Box::<i32>::new;
        let five = box_new(5);
        assert!(*five == 5);
    });
    let expected = format_js(
        r#"
            let five = 5;
            console.assert(five === 5);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "low priority"]
#[tokio::test]
async fn boxed_iter_type_infer() {
    let actual = r2j_block!({
        // Need to take into account box generics??:
        let iter = [1, 2, 3].into_iter().collect();
        let boxed_vec = Box::<Vec<i32>>::new(iter);
        assert!(boxed_vec[0] == 1);
    });
    let expected = format_js(
        r#"
            let five = 5;
            console.assert(five === 5);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn array() {}

#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[allow(unused_variables)]
#[allow(clippy::disallowed_names)]
#[ignore = "TODO LOW PRIORITY JS class and let declarations clash"]
// TODO where items and vars in the same scope have the same name, Rust seems to intelligently figure which to use based on how the thing is being used eg are we calling a method on it or adding a number to it. JS doesn't allow defining a let variable followed by a class with the same name, so we need to use analysis to determine when to either rename the variable/item or use `var` in that case and handle the scoping issues that that brings (the reason we started using let).
#[tokio::test]
async fn resolve_path_to_item_vs_var_same_scope() {
    let actual = r2j_block!({
        struct foo {
            num: i32,
        }
        let foo = 1;
        let bar = 2;
        struct bar {
            num: i32,
        }
        impl bar {
            fn get_num() -> i32 {
                3
            }
        }
        assert!(foo == 1);
        assert!(bar::get_num() == 3);
    });
    let expected = format_js(
        r#"
            class foo {
                constructor(num) {
                    this.num = num;
                }
            }
            let foo = 1;
            let bar = 2;
            class bar {
                constructor(num) {
                    this.num = num;
                }

                static getNum() {
                    return 3;
                }
            }

            console.assert(foo === 1);
            console.assert(bar.getNum() === 3);
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[allow(unused_variables)]
#[tokio::test]
// Ensure we are correctly looking up paths scope by scope rather than eg looking for vars in all scopes *then* looking for item definitions in all scopes.
async fn resolve_path_to_item_vs_var_parent_scopes() {
    let actual = r2j_block!({
        struct foo {
            num: i32,
        }
        let bar = 2;

        fn inner_scope() {
            let foo = 1;
            struct bar {
                num: i32,
            }
            impl bar {
                fn get_num() -> i32 {
                    3
                }
            }
            assert!(foo == 1);
            assert!(bar::get_num() == 3);
        }

        inner_scope();
    });
    let expected = format_js(
        r#"
        class foo {
            constructor(num) {
              this.num = num;
            }
        }
        let bar = 2;
        function innerScope() {
            let foo = 1;
            class bar {
                constructor(num) {
                  this.num = num;
                }
  
                static getNum() {
                  return 3;
                }
            }
  
            console.assert(foo === 1);
            console.assert(bar.getNum() === 3);
        }
        innerScope();
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// Duplicate names
#[allow(clippy::assertions_on_constants)]
#[tokio::test]
async fn const_duplicate_name() {
    let actual = r2j_file_run_main!(
        const FOO: i32 = 1;
        mod foo {
            pub const FOO: i32 = 2;
        }
        fn main() {
            assert!(FOO == 1);
            assert!(foo::FOO == 2);
        }
    );

    let expected = format_js(
        r#"
            // crate
            const FOO = 1;
            function main() {
                console.assert(FOO === 1);
                console.assert(foo__FOO === 2);
            }

            // foo
            const foo__FOO = 2;

            main();
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn enum_duplicate_name() {
    let actual = r2j_file_run_main!(
        enum Foo {
            Bar,
        }
        mod foo {
            pub enum Foo {
                Bar,
            }
        }
        fn main() {
            let foo = Foo::Bar;
            let foo2 = foo::Foo::Bar;
            // TODO add support for PartialEq or matches!
            // assert!(foo == Foo::Bar);
            // assert!(matches!(foo2, foo::Foo::Bar));
        }
    );

    let expected = format_js(
        r#"
            // crate
            class Foo {
                static barId = "Bar";
                static Bar = new Foo("Bar", null);
                constructor(id, data) {
                    this.id = id;
                    this.data = data;
                }
            }
            function main() {
                let foo = Foo.Bar;
                let foo2 = foo__Foo.Bar;
            }

            // foo
            class foo__Foo {
                static barId = "Bar";
                static Bar = new Foo("Bar", null);
                constructor(id, data) {
                    this.id = id;
                    this.data = data;
                }
            }

            main();
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}
