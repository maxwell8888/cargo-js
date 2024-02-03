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
                return this.age.add(new RustInteger(10));
            }
            static greeting() {
                return new RustString("Hello").toString();
            }
            agePlusTwenty() {
                return this.agePlusTen().add(new RustInteger(10));
            }
        }

        var bob = new Person(new RustInteger(30));
        console.assert(Person.greeting().eq(new RustString("Hello").toString()).jsBoolean);
        console.assert(bob.agePlusTen().eq(new RustInteger(40)).jsBoolean);
        console.assert(bob.agePlusTwenty().eq(new RustInteger(50)).jsBoolean);
        "#,
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
