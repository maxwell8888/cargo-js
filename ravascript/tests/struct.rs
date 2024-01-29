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
async fn it_transpiles_struct_no_new() {
    #[module_as_str]
    mod wrapper {
        struct MyStruct {
            my_field: i32,
        }
    }
    let expected = r#"class MyStruct {
  constructor(myField) {
    this.myField = myField;
  }
}"#;
    assert_eq!(expected, generated_js());
}

#[tokio::test]
async fn struct_and_impl_methods() {
    let actual = r2j_block_with_prelude!({
        trait MyTrait {
            fn get_age(&self) -> i32;
        }
        struct MyStruct {
            age: i32,
            name: &'static str,
        }
        impl MyStruct {
            fn new(age: i32, name: &'static str) -> MyStruct {
                MyStruct { age, name }
            }
            fn my_method(&self) -> &str {
                self.name
            }
            fn my_method_with_arg(&self, inc: i32) -> i32 {
                self.age + inc
            }
            fn my_associated_method(inc: i32) -> i32 {
                inc + 10
            }
            fn with_generic<T>(&self, inc: T) -> i32 {
                self.age
            }
        }
        // TODO currently impls for a struct must appear directly after it. defining a trait inbetween like below is not suppported
        // trait MyTrait {
        //     fn get_age(&self) -> i32;
        // }
        impl MyTrait for MyStruct {
            fn get_age(&self) -> i32 {
                self.age
            }
        }

        let thing = MyStruct::new(2, "Bruce");
        // Console::assert(thing.my_method() == "Bruce");
        // Console::assert(thing.my_method_with_arg(2) == 4);
        // Console::assert(MyStruct::my_associated_method(2) == 12);
        // Console::assert(thing.with_generic(99) == 2);
        // Console::assert(thing.get_age() == 2);

        // console.assert(thing.myMethod() === "Bruce");
        // console.assert(thing.myMethodWithArg(2) === 4);
        // console.assert(MyStruct.myAssociatedMethod(2) === 12);
        // console.assert(thing.withGeneric(99) === 2);
        // console.assert(thing.getAge() === 2);

        assert_eq!(thing.my_method(), "Bruce");
        assert_eq!(thing.my_method_with_arg(2), 4);
        assert_eq!(MyStruct::my_associated_method(2), 12);
        assert_eq!(thing.with_generic(99), 2);
        assert_eq!(thing.get_age(), 2);
    });
    let expected = concat!(
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_string_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"
        class MyStruct {
            constructor(age, name) {
                this.age = age;
                this.name = name;
            }
        
            static new(age, name) {
                return new MyStruct(age, name);
            }
            myMethod() {
                return this.name;
            }
            myMethodWithArg(inc) {
                return this.age.add(inc);
            }
            static myAssociatedMethod(inc) {
                return inc.add(new RustInteger(10));
            }
            withGeneric(inc) {
                return this.age;
            }
            getAge() {
                return this.age;
            }
        }

        var thing = MyStruct.new(new RustInteger(2), new RustString("Bruce"));
        console.assert(thing.myMethod().eq(new RustString("Bruce")));
        console.assert(thing.myMethodWithArg(new RustInteger(2)).eq(new RustInteger(4)));
        console.assert(MyStruct.myAssociatedMethod(new RustInteger(2)).eq(new RustInteger(12)));
        console.assert(thing.withGeneric(new RustInteger(99)).eq(new RustInteger(2)));
        console.assert(thing.getAge().eq(new RustInteger(2)));        
        "#
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn impl_in_fn_scope() {
    // impls can be inside lower *scopes* (not modules) eg inside functions (and the functions don't even need to be run)
    let actual = r2j_block!({
        struct Cool {}
        if false {
            fn inner() {
                impl Cool {
                    fn whatever(&self) -> i32 {
                        5
                    }
                }
            }
        }
        let cool = Cool {};
        assert_eq!(cool.whatever(), 5)
    });
    let expected = r#"
    class Cool {
        whatever() {
            return new RustInteger(5);
        }
    }
    if (new RustBool(false).jsBoolean) {
        function inner() {}
    }
    var cool = new Cool();
    console.assert(cool.whatever().eq(new RustInteger(5)));
    "#;
    assert_eq!(format_js(expected), actual);
}

#[tokio::test]
async fn tuple_struct() {
    let actual = r2j_block_with_prelude!({
        struct Cool(i32);
        impl Cool {
            fn get_inner(&self) -> i32 {
                self.0
            }
            fn other_number(&self) -> i32 {
                4
            }
        }
        let cool = Cool(5);
        assert_eq!(cool.0, 5);
        assert_eq!(cool.get_inner(), 5);
        assert_eq!(cool.other_number(), 4);
    });
    let expected = concat!(
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"class Cool {
            constructor(arg0) {
                this[0] = arg0;
            }

            getInner() {
                return this[0];
            }
            otherNumber() {
                return new RustInteger(4);
            }
        }
        
        var cool = new Cool(new RustInteger(5));
        console.assert(cool[0].eq(new RustInteger(5)));
        console.assert(cool.getInner().eq(new RustInteger(5)));
        console.assert(cool.otherNumber().eq(new RustInteger(4)));
        "#
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn tuple_struct_multiple_fields() {
    let actual = r2j_block_with_prelude!({
        struct Cool(i32, String, bool, i32);
        impl Cool {
            fn zero(&self) -> i32 {
                self.0
            }
            fn one(&self) -> String {
                self.1.clone()
            }
            fn two(&self) -> bool {
                self.2
            }
            fn three(&self) -> i32 {
                self.3
            }
        }
        let cool = Cool(5, "hi".to_string(), true, 4);
        assert_eq!(cool.0, 5);
        assert_eq!(cool.1, "hi".to_string());
        assert_eq!(cool.2, true);
        assert_eq!(cool.3, 4);
    });
    let expected = concat!(
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_string_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"class Cool {
            constructor(arg0, arg1, arg2, arg3) {
                this[0] = arg0;
                this[1] = arg1;
                this[2] = arg2;
                this[3] = arg3;
            }

            zero() {
                return this[0];
            }
            one() {
                return this[1].clone();
            }
            two() {
                return this[2];
            }
            three() {
                return this[3];
            }
        }

        var cool = new Cool(new RustInteger(5), new RustString("hi").toString(), new RustBool(true), new RustInteger(4));
        console.assert(cool[0].eq(new RustInteger(5)));
        console.assert(cool[1].eq(new RustString("hi").toString()));
        console.assert(cool[2].eq(new RustBool(true)));
        console.assert(cool[3].eq(new RustInteger(4)));
        "#
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
