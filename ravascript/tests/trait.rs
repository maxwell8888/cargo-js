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

#[ignore]
#[tokio::test]
async fn impl_in_fn_scope() {
    let actual = r2j_block_with_prelude!({
        trait Interaction {
            fn greeting() -> String {
                "Hello".to_string()
            }
            fn age_plus_twenty(&self) -> i32 {
                self.age_plus_ten() + 10
            }
            fn age_plus_ten(&self) -> i32;
        }
        struct Person {
            age: i32,
        }
        impl Interaction for Person {
            fn age_plus_ten(&self) -> i32 {
                self.age + 10
            }
        }
        let bob = Person { age: 30 };
        assert_eq!(Person::greeting(), "Hello".to_string());
        assert_eq!(bob.age_plus_ten(), 40);
        assert_eq!(bob.age_plus_twenty(), 50);
    });
    let expected = concat!(
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_string_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"
        class Person {
            constructor(age) {
                this.age = age;
            }

            agePlusTen() {
                return this.age.add(10);
            }
            static greeting() {
                return "Hello";
            }
            agePlusTwenty() {
                return this.agePlusTen().add(10);
            }
        }

        let bob = new Person(30);
        console.assert(Person.greeting().eq("Hello"));
        console.assert(bob.agePlusTen().eq(40));
        console.assert(bob.agePlusTwenty().eq(50));
        "#,
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore]
#[tokio::test]
async fn impl_with_generic_arguments() {
    let actual = r2j_block_with_prelude!({
        pub enum Double<T: PartialEq> {
            One(T),
            Two,
        }
        impl<T: PartialEq> Double<T> {
            pub fn foo<F>(self, f: F) -> T
            where
                F: FnOnce() -> T,
            {
                match self {
                    Double::One(x) => x,
                    Double::Two => f(),
                }
            }

            // pub fn map<U: PartialEq, F>(self, f: F) -> Option<U>
            // where
            //     F: FnOnce(T) -> U,
            // {
            //     match self {
            //         Some(x) => Some(f(x)),
            //         None => None,
            //     }
            // }
        }

        let one = Double::One(5);
        let two: Double<i32> = Double::Two;
        let one_result = one.foo(|| 4);
        let two_result = two.foo(|| 4);
        assert_eq!(one_result, 5);
        assert_eq!(two_result, 4);
    });
    let expected = concat!(
        // include_str!("rust_integer_prelude.js"),
        // include_str!("rust_string_prelude.js"),
        // include_str!("rust_bool_prelude.js"),
        r#"
        class Double {
            static TwoId = "Two";
            static Two = new Double("Two", null);
            static OneId = "One";
            constructor(id, data) {
                this.id = id;
                this.data = data;
            }
            static One(arg_0, arg_1) {
                return new Double("One", [arg_0, arg_1]);
            }
            foo(f) {
                var ifTempAssignment;
                if (this.id === Double.OneId) {
                    var [text, num] = this.data;
                    ifTempAssignment = num;
                } else if (this.id === Double.TwoId) {
                    ifTempAssignment = f();
                } else {
                    throw new Error("couldn't match enum variant");
                }
                return ifTempAssignment;
            }
        }

        const one = Double.One(5);
        const two = Double.Two();
        const oneResult = one.foo(() => 4);
        const twoResult = two.foo(() => 4);
        console.assert(oneResult === 5);
        console.assert(twoResult === 4);
        "#,
    );
    // assert!(false);
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
