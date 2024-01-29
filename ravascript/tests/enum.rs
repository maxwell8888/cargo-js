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
async fn enum_match() {
    let actual = r2j_block_with_prelude!({
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
            MyEnum::Bar { x, y } => {
                Console::log(x);
                Console::log(y);
                x
            }
            MyEnum::Baz(text, num) => {
                Console::log(text);
                Console::log(num);
                num
            }
        };
        assert_eq!(match_result, 5);
    });

    let expected = concat!(
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_string_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"class MyEnum {
            static fooBarId = "FooBar";
            static FooBar = new MyEnum("FooBar", null);
            static barId = "Bar";
            static bazId = "Baz";
            constructor(id, data) {
                this.id = id;
                this.data = data;
            }
            static Bar(data) {
                return new MyEnum("Bar", data);
            }
            static Baz(arg_0, arg_1) {
                return new MyEnum("Baz", [arg_0, arg_1]);
            }
        }
        var myData = MyEnum.FooBar;
        var myData = MyEnum.Bar({
            x: new RustInteger(4),
            y: new RustString("Hello"),
        });
        var myData = MyEnum.Baz(new RustString("Hi"), new RustInteger(5));
        var matchResult;
        if (myData.id === MyEnum.fooBarId) {
            matchResult = new RustInteger(1);
        } else if (myData.id === MyEnum.barId) {
            var { x, y } = myData.data;
            console.log(x);
            console.log(y);
            matchResult = x;
        } else if (myData.id === MyEnum.bazId) {
            var [text, num] = myData.data;
            console.log(text);
            console.log(num);
            matchResult = num;
        } else {
            throw new Error("couldn't match enum variant");
        }
        console.assert(matchResult.eq(new RustInteger(5)));
        "#
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn enum_methods() {
    let actual = r2j_block_with_prelude!({
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
            fn bar_y(&self) -> Option<String> {
                match self {
                    Animal::Cat => None,
                    Animal::Dog => None,
                    Animal::Bar { x, y } => Some(y.to_string()),
                    Animal::Baz(text, num) => None,
                }
            }
            fn baz_num(&self) -> i32 {
                match self {
                    Animal::Cat => 0,
                    Animal::Dog => 0,
                    Animal::Bar { x, y } => 0,
                    Animal::Baz(text, num) => *num,
                }
            }
        }
        // TODO this is being transpiled to `{ x: 1, y: "hibar" }` which doesn't have a `.eq()` so can't compare eg Some(5) = Some(5)
        // should get rid of the special eq for Option and use JSON.stringify
        let bar = Animal::Bar { x: 1, y: "hibar" };
        let baz = Animal::Baz("hibaz", 5);

        // Some("hibar")
        let bar_y = bar.bar_y();
        assert_eq!(bar_y, Some("hibar".to_string()));

        // None
        let bar_y2 = baz.bar_y();
        assert_eq!(bar_y2, None);

        // Some("hibar")
        let baz_num = bar.baz_num();
        assert_eq!(baz_num, 0);

        // None
        let baz_num = baz.baz_num();
        assert_eq!(baz_num, 5);
    });

    let expected = concat!(
        include_str!("option_prelude.js"),
        "var Some = Option.Some;
        var None = Option.None;",
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_string_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"class Animal {
            static catId = "Cat";
            static Cat = new Animal("Cat", null);
            static dogId = "Dog";
            static Dog = new Animal("Dog", null);
            static barId = "Bar";
            static bazId = "Baz";
            constructor(id, data) {
                this.id = id;
                this.data = data;
            }
            static Bar(data) {
                return new Animal("Bar", data);
            }
            static Baz(arg_0, arg_1) {
                return new Animal("Baz", [arg_0, arg_1]);
            }
            static five() {
                return new RustInteger(5);
            }
            barY() {
                var ifTempAssignment;
                if (this.id === Animal.catId) {
                    ifTempAssignment = None;
                } else if (this.id === Animal.dogId) {
                    ifTempAssignment = None;
                } else if (this.id === Animal.barId) {
                    var { x, y } = this.data;
                    ifTempAssignment = Some(y.toString());
                } else if (this.id === Animal.bazId) {
                    var [text, num] = this.data;
                    ifTempAssignment = None;
                } else {
                    throw new Error("couldn't match enum variant");
                }
                return ifTempAssignment;
            }
            bazNum() {
                var ifTempAssignment;
                if (this.id === Animal.catId) {
                    ifTempAssignment = new RustInteger(0);
                } else if (this.id === Animal.dogId) {
                    ifTempAssignment = new RustInteger(0);
                } else if (this.id === Animal.barId) {
                    var { x, y } = this.data;
                    ifTempAssignment = new RustInteger(0);
                } else if (this.id === Animal.bazId) {
                    var [text, num] = this.data;
                    ifTempAssignment = num;
                } else {
                    throw new Error("couldn't match enum variant");
                }
                return ifTempAssignment;
            }
        }
        
        var bar = Animal.Bar({
            x: new RustInteger(1),
            y: new RustString("hibar"),
        });
        var baz = Animal.Baz(new RustString("hibaz"), new RustInteger(5));
        var barY = bar.barY();
        console.assert(barY.eq(Some(new RustString("hibar").toString())));
        var barY2 = baz.barY();
        console.assert(barY.eq(None));
        var bazNum = bar.bazNum();
        console.assert(bazNum.eq(new RustInteger(0)));
        var bazNum = baz.bazNum();
        console.assert(bazNum.eq(new RustInteger(5)));        
        "#
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
