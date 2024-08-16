use pretty_assertions::assert_eq;
use ravascript_core::{format_js, from_crate};

use super::utils::*;
use crate::{r2j_file, r2j_file_run_main, r2j_file_unchecked};

// #[ignore]
#[tokio::test]
async fn it_transpiles_crate_directory() {
    setup_tracing();

    let actual = from_crate("../for-testing".into(), false, true);
    let actual = format_js(actual);

    let expected = format_js(
        r#"
            // crate
            function crate__duplicateName() {
                return 10;
            }
            function main() {
                crate__duplicateName();
                utils__colors__duplicateName();
                let thing = External.new();
                let fido = new Green(true, 2);
                fido.woof();
            };
  
            // foo_bar
            class Internal {
                constructor(age) {
                    this.age = age;
                }
              
                addTen() {
                    return this.age + 10;
                }
            }
            
            class External {
                constructor(sub, count) {
                    this.sub = sub;
                    this.count = count;
                }
              
                static new() {
                    return new External(new Internal(0), 9);
                }
            }
  
            // foo_bar::file_module_level_inline
            function fileModuleLevelInline__duplicateName() {
                return 12;
            }
  
            // foo_bar::file_module_level_inline::file_module_inline_sub
            function fileModuleInlineSub__duplicateName() {
                return 13;
            }
  
            // colors
            const DOG_ACTIVITY = 5;
            function crate__colors__duplicateName() {
                return 6;
            }
            function stuffFunction() {
                return 4;
            }
  
            // colors::green
            function green__duplicateName() {
                return 3;
            }
            class Green {
                constructor(fluffy, age) {
                    this.fluffy = fluffy;
                    this.age = age;
                }
              
                woof() {
                    function duplicateName() {
                        return 9;
                    }
                    console.assert(this.age === 2);
                    console.assert(green__duplicateName() === 3);
                    console.assert(duplicateName() === 9);
                    console.assert(sayHello() === 8);
                    console.assert(crate__duplicateName() === 10);
                    console.assert(sayHello() === 8);
                    console.assert(green__duplicateName() === 3);
                    console.assert(crate__colors__duplicateName() === 6);
                    console.assert(utils__colors__duplicateName() === 7);
                    console.assert(DOG_ACTIVITY === 5);
                    console.assert(stuffFunction() === 4);
                    console.assert(crateLevelInline__duplicateName() === 1);
                    console.assert(crateLevelInlineSub__duplicateName() === 11);
                    console.assert(fileModuleLevelInline__duplicateName() === 12);
                    console.assert(fileModuleInlineSub__duplicateName() === 13);
                }
            }
  
            // utils

            // utils::colors
            function utils__colors__duplicateName() {
                return 7;
            }
            
            // utils::say_something
            function sayHello() {
                return 8;
            }
  
            // crate_level_inline
            function crateLevelInline__duplicateName() {
                return 1;
            }
            
            // crate_level_inline::crate_level_inline_sub
            function crateLevelInlineSub__duplicateName() {
                return 11;
            }
  
            main();
        "#,
    );

    // println!("{}", expected);
    // println!("{}", format_js(expected));
    // println!("{}", &actual);
    // println!("{}", format_js(&actual));

    assert_eq!(expected, format_js(actual));
    execute_js_with_assertions(&expected).await.unwrap();
}

// TODO it might be better to rely on the for-testing dir for testing `crate` rather than using unchecked Rust
#[allow(dead_code)]
#[tokio::test]
async fn simple_module() {
    // let actual = r2j_file_unchecked!(
    // TODO I think I would actually prefer to have an explicit `mod wrapper { }` in cases like this. Whilst it is more verbose, it makes it much clearer what `self` if referring to.
    let actual = r2j_file!(
        struct Bar {}
        pub fn baz() {
            let _ = Bar {};
        }
        mod foo {
            fn green() {
                // let blue = crate::baz();
            }
        }
    );
    let expected = format_js(
        r#"
            // crate
            class Bar {}
            function baz() {
                let _ = new Bar();
            }
  
            // foo
            function green() {}
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn duplicate_names_impl_scope() {
    // let actual = r2j_file_unchecked!(
    // TODO I think I would actually prefer to have an explicit `mod wrapper { }` in cases like this. Whilst it is more verbose, it makes it much clearer what `self` if referring to.
    let actual = r2j_file!(
        fn foo() {}
        mod bar {
            fn foo() {}

            struct Green {}
            impl Green {
                pub fn bar() {
                    fn foo() {}
                    foo();
                }
            }
        }
    );
    // let expected = format_js(
    //     r#"
    //         // crate
    //         function crate__foo() {}

    //         // bar
    //         function bar__foo() {}
    //         stru
    //         function bar() {
    //             function foo() {}
    //             foo();
    //         }

    //     "#,
    // );
    let expected = format_js(r#""#);
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[allow(dead_code, clippy::let_unit_value, unused_variables)]
#[tokio::test]
async fn module_super() {
    // let actual = r2j_file_unchecked!(
    // TODO I think I would actually prefer to have an explicit `mod wrapper { }` in cases like this. Whilst it is more verbose, it makes it much clearer what `self` if referring to.
    let actual = r2j_file!(
        struct Bar {}
        fn baz() {
            let _ = Bar {};
        }
        mod foo {
            fn green() {
                let blue = super::baz();
            }
        }
    );
    let expected = format_js(
        r#"
            // crate
            class Bar {}
            function baz() {
                let _ = new Bar();
            }
            
            // foo
            function green() {
                let blue = baz();
            }
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[allow(dead_code, clippy::let_unit_value, unused_variables)]
#[tokio::test]
async fn module_self() {
    let actual = r2j_file!(
        struct Bar {}
        fn baz() {
            let _ = Bar {};
        }
        fn green() {
            let blue = self::baz();
        }
    );
    let expected = format_js(
        r#"
            class Bar {}
            function baz() {
                let _ = new Bar();
            }
            function green() {
                let blue = baz();
            }
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn module_crate() {
    // Can't be checked because `crate::` will be pointing at the wrong thing
    let actual = r2j_file_unchecked!(
        fn baz() -> i32 {
            5
        }
        mod foo {
            fn green() {
                let blue = crate::baz();
            }
        }
    );
    let expected = format_js(
        r#"
            // crate
            function baz() {
                return 5;
            }
            
            // foo
            function green() {
                let blue = baz();
            }
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// TODO the Rust code is not being run, causing an assert! to fail is not reported - SOLUTION need to use r2j_file_run_main not r2j_file
// TODO write some explicit tests for r2j_file! to check it fails where expected
#[tokio::test]
async fn use_paths() {
    let actual = r2j_file_run_main!(
        fn duplicate() -> i32 {
            0
        }
        mod one {
            pub fn duplicate() -> i32 {
                1
            }
            pub use two::three;
            pub mod two {
                fn duplicate() -> i32 {
                    2
                }
                pub mod three {
                    // Access private items in parent modules
                    pub fn duplicate() -> i32 {
                        super::super::duplicate() + super::duplicate()
                    }
                    pub mod another_one {
                        pub use super::super::super::duplicate;
                    }
                }
            }
        }
        mod four {
            use super::one;
            pub use one::two;
            pub fn duplicate() -> i32 {
                two::three::duplicate() + one::duplicate()
            }
        }
        mod five {
            use super::four::two;
            use two::three;
            pub fn duplicate() -> i32 {
                two::three::duplicate()
                    + super::one::three::another_one::duplicate()
                    + three::another_one::duplicate()
            }
        }
        fn main() {
            assert!(duplicate() == 0);
            assert!(one::duplicate() == 1);
            assert!(one::two::three::duplicate() == 3);
            assert!(four::duplicate() == 4);
            assert!(five::duplicate() == 5);
        }
    );
    let actual = format_js(actual);

    let expected = format_js(
        r#"
            // crate
            function duplicate() {
                return 0;
            }
            function main() {
                console.assert(duplicate() === 0);
                console.assert(one__duplicate() === 1);
                console.assert(three__duplicate() === 3);
                console.assert(four__duplicate() === 4);
                console.assert(five__duplicate() === 5);
            }
  
            // one
            function one__duplicate() {
                return 1;
            }
  
            // one::two
            function two__duplicate() {
                return 2;
            }
  
            // one::two::three
            function three__duplicate() {
                return one__duplicate() + two__duplicate();
            }
  
            // one::two::three::another_one
  
            // four
            function four__duplicate() {
                return three__duplicate() + one__duplicate();
            }
  
            // five
            function five__duplicate() {
                return three__duplicate() + one__duplicate() + one__duplicate();
            }
  
            main();
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&actual).await.unwrap();
}

// TODO IMPORTANT scoped modules. I think scoped modules might break
#[ignore = "TODO"]
#[tokio::test]
async fn scoped_modules() {
    let actual = r2j_file_run_main!(
        fn one() -> i32 {
            mod foo {
                pub fn bar() -> i32 {
                    1
                }
            }
            foo::bar()
        }
        // fn two() -> i32 {
        //     mod foo {
        //         pub fn bar() -> i32 {
        //             2
        //         }
        //     }
        //     foo::bar()
        // }
        fn main() {
            assert!(one() == 1);
            // assert!(two() == 2);
        }
    );

    let expected = format_js(
        r#"
            // crate
            function one() {
                function bar() {
                    return 1;
                }
                return bar();
            }
            function two() {
                function bar() {
                    return 1;
                }
                return bar();
            }
            function main() {
              console.assert(one() === 1);
              console.assert(two() === 2);
            }

            main();
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "TODO"]
#[tokio::test]
async fn scoped_modules_duplicate_names() {
    let actual = r2j_file_run_main!(
        fn foo() -> i32 {
            mod one {
                pub fn num() -> i32 {
                    1
                }
            }
            mod two {
                pub fn num() -> i32 {
                    2
                }
            }
            one::num() + two::num()
        }
        fn main() {
            assert!(foo() == 3);
        }
    );

    let expected = format_js(
        r#"
            // crate
            function foo() {
                function one__num() {
                    return 1;
                }
                function two__num() {
                    return 2;
                }
                return one__num() + two__num();
            }
            function main() {
              console.assert(foo() === 3);
            }

            main();
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "TODO"]
#[tokio::test]
async fn scoped_use() {
    let actual = r2j_file_run_main!(
        fn duplicate() -> i32 {
            use one::duplicate;
            assert!(duplicate() == 1);
            5
        }
        mod one {
            pub fn duplicate() -> i32 {
                1
            }
        }
        fn main() {
            duplicate();
        }
    );

    let expected = format_js(
        r#"
            // crate
            function duplicate() {
                console.assert(one__duplicate() === 1);
                return 5;
            }
            function main() {
                duplicate();
            }

            // one
            function one__duplicate() {
                return 1;
            }

            main();
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}
