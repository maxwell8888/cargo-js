use pretty_assertions::assert_eq;
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;

use super::utils::*;
use crate::r2j_block;

#[allow(unused_variables)]
#[tokio::test]
async fn enum_match() {
    setup_tracing();
    // let actual = r2j_block_unformatted!({
    let actual = r2j_block!({
        enum MyEnum {
            FooBar,
            // Foo(MyType),
            Bar { x: i32, y: &'static str },
            Baz(&'static str, i32),
        }
        let my_data = MyEnum::FooBar;
        let my_data = MyEnum::Bar { x: 4, y: "Hello" };
        let my_data = MyEnum::Baz("Hi", 5);
        // TODO need to use a better pretty printer cos the current one messes up the the destructure formatting
        let match_result = match my_data {
            MyEnum::FooBar => 1,
            // NOTE how even though there is no block to allow eg a struct to be defined, we still have a scope because we var x is used which is only available in this scope, not the outer scope.
            MyEnum::Bar { x, y } => x,
            MyEnum::Baz(text, num) => {
                // Comment to force block
                num
            }
        };
        assert!(match_result == 5);
    });

    // println!("{actual}");

    // include_str!("string_prototype_extensions.js"),
    // "\n",
    // include_str!("number_prototype_extensions.js"),
    let expected = format_js(
        r#"
            class MyEnum {
                static FooBarId = "FooBar";
                static FooBar = new MyEnum("FooBar", null);
                static BarId = "Bar";
                static BazId = "Baz";
                constructor(id, data) {
                    this.id = id;
                    this.data = data;
                }
                static Bar(data) {
                    return new MyEnum("Bar", data);
                }
                static Baz(arg0, arg1) {
                    return new MyEnum("Baz", [arg0, arg1]);
                }
            }
            let myData = MyEnum.FooBar;
            myData = MyEnum.Bar({
                x: 4,
                y: "Hello",
            });
            myData = MyEnum.Baz("Hi", 5);
            var matchResult;
            if (myData.id === MyEnum.FooBarId) {
                matchResult = 1;
            } else if (myData.id === MyEnum.BarId) {
                var { x, y } = myData.data;
                matchResult = x;
            } else if (myData.id === MyEnum.BazId) {
                var [text, num] = myData.data;
                matchResult = num;
            } else {
                throw new Error("couldn't match enum variant");
            }
            console.assert(matchResult === 5);
            "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn enum_methods() {
    setup_tracing();
    let actual = r2j_block!({
        enum Animal {
            Cat,
            Dog,
            Bar { x: i32, y: &'static str },
            Baz(&'static str, i32),
        }
        impl Animal {
            fn five() -> i32 {
                5
            }
            fn bar_y(self) -> &'static str {
                match self {
                    Animal::Cat => "Cat",
                    Animal::Dog => "Dog",
                    Animal::Bar { x, y } => y,
                    Animal::Baz(text, num) => text,
                }
            }
            fn baz_num(self) -> i32 {
                match self {
                    Animal::Cat => 0,
                    Animal::Dog => 0,
                    Animal::Bar { x, y } => 0,
                    Animal::Baz(text, num) => num,
                }
            }
        }
        // TODO this is being transpiled to `{ x: 1, y: "hibar" }` which doesn't have a `.eq()` so can't compare eg Some(5) = Some(5)
        // should get rid of the special eq for Option and use JSON.stringify
        let bar = Animal::Bar { x: 1, y: "hibar" };
        let baz = Animal::Baz("hibaz", 5);

        let bar_y = bar.bar_y();
        assert!(bar_y == "hibar");

        let bar_y_two = baz.bar_y();
        assert!(bar_y_two == "hibaz");

        // Redeclare vars because they have been moved
        let bar = Animal::Bar { x: 1, y: "hibar" };
        let baz = Animal::Baz("hibaz", 5);

        let baz_num = bar.baz_num();
        assert!(baz_num == 0);

        let baz_num = baz.baz_num();
        assert!(baz_num == 5);
    });

    // include_str!("option_prelude.js"),
    // "let Some = Option.Some;",
    // "let None = Option.None;",
    // include_str!("string_prototype_extensions.js"),
    // "\n",
    // include_str!("number_prototype_extensions.js"),
    let expected = format_js(concat!(
        r#"
        class Animal {
            static CatId = "Cat";
            static Cat = new Animal("Cat", null);
            static DogId = "Dog";
            static Dog = new Animal("Dog", null);
            static BarId = "Bar";
            static BazId = "Baz";
            constructor(id, data) {
                this.id = id;
                this.data = data;
            }
            static Bar(data) {
                return new Animal("Bar", data);
            }
            static Baz(arg0, arg1) {
                return new Animal("Baz", [arg0, arg1]);
            }
            static five() {
                return 5;
            }
            barY() {
                var ifTempAssignment;
                if (this.id === Animal.CatId) {
                    ifTempAssignment = "Cat";
                } else if (this.id === Animal.DogId) {
                    ifTempAssignment = "Dog";
                } else if (this.id === Animal.BarId) {
                    var { x, y } = this.data;
                    ifTempAssignment = y;
                } else if (this.id === Animal.BazId) {
                    var [text, num] = this.data;
                    ifTempAssignment = text;
                } else {
                    throw new Error("couldn't match enum variant");
                }
                return ifTempAssignment;
            }
            bazNum() {
                var ifTempAssignment;
                if (this.id === Animal.CatId) {
                    ifTempAssignment = 0;
                } else if (this.id === Animal.DogId) {
                    ifTempAssignment = 0;
                } else if (this.id === Animal.BarId) {
                    var { x, y } = this.data;
                    ifTempAssignment = 0;
                } else if (this.id === Animal.BazId) {
                    var [text, num] = this.data;
                    ifTempAssignment = num;
                } else {
                    throw new Error("couldn't match enum variant");
                }
                return ifTempAssignment;
            }
        }
        
        let bar = Animal.Bar({
            x: 1,
            y: "hibar",
        });
        let baz = Animal.Baz("hibaz", 5);
        let barY = bar.barY();
        console.assert(barY === "hibar");
        let barYTwo = baz.barY();
        console.assert(barYTwo === "hibaz");
        bar = Animal.Bar({
            x: 1,
            y: "hibar",
        });
        baz = Animal.Baz("hibaz", 5);
        let bazNum = bar.bazNum();
        console.assert(bazNum === 0);
        bazNum = baz.bazNum();
        console.assert(bazNum === 5);
        "#
    ));
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}
