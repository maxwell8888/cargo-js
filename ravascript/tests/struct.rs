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
    setup_tracing();

    let actual = r2j_block_with_prelude!({
        struct MyStruct {
            my_field: i32,
        }
    });
    let expected = format_js(
        r#"
            class MyStruct {
                constructor(myField) {
                    this.myField = myField;
                }
            }
        "#,
    );
    assert_eq!(expected, actual);
}

// #[ignore = "wait to support impl Trait for T"]
#[tokio::test]
async fn struct_and_impl_methods() {
    setup_tracing();

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
            // fn with_generic<T>(&self, inc: T) -> i32 {
            //     self.age
            // }
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

        assert!(thing.my_method() == "Bruce");
        assert!(thing.my_method_with_arg(2) == 4);
        assert!(MyStruct::my_associated_method(2) == 12);
        // assert_eq!(thing.with_generic::<i32>(99), 2);
        assert!(thing.get_age() == 2);
    });
    // withGeneric(inc) {
    //     return this.age;
    // }
    // console.assert(thing.withGeneric(99).eq(2));
    let expected = format_js(
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
                    return this.age + inc;
                }
                static myAssociatedMethod(inc) {
                    return inc + 10;
                }
                getAge() {
                    return this.age;
                }
            }

            let thing = MyStruct.new(2, "Bruce");
            console.assert(thing.myMethod() === "Bruce");
            console.assert(thing.myMethodWithArg(2) === 4);
            console.assert(MyStruct.myAssociatedMethod(2) === 12);
            console.assert(thing.getAge() === 2);
        "#,
    );
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
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
        assert!(cool.0 == 5);
        assert!(cool.get_inner() == 5);
        assert!(cool.other_number() == 4);
    });

    // include_str!("rust_integer_prelude.js"),
    // include_str!("rust_bool_prelude.js"),
    let expected = concat!(
        r#"class Cool {
            constructor(arg0) {
                this[0] = arg0;
            }

            getInner() {
                return this[0];
            }
            otherNumber() {
                return 4;
            }
        }
        
        let cool = new Cool(5);
        console.assert(cool[0] === 5);
        console.assert(cool.getInner() === 5);
        console.assert(cool.otherNumber() === 4);
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
        assert!(cool.0 == 5);
        assert!(cool.1 == "hi".to_string());
        assert!(cool.2 == true);
        assert!(cool.3 == 4);
    });
    // include_str!("rust_integer_prelude.js"),
    // include_str!("rust_string_prelude.js"),
    // include_str!("rust_bool_prelude.js"),
    let expected = concat!(
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
                return this[1];
            }
            two() {
                return this[2];
            }
            three() {
                return this[3];
            }
        }

        let cool = new Cool(5, "hi", true, 4);
        console.assert(cool[0] === 5);
        console.assert(cool[1] === "hi");
        console.assert(cool[2] === true);
        console.assert(cool[3] === 4);
        "#
    );
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn mutate_non_copy_struct() {
    // We cannot reuse a moved (ie assigned to var, passed to fn) var, so don't need to worry about mutations affecting orginal variable
    let actual = r2j_block_with_prelude!({
        struct Foo {
            num: i32,
        }
        let mut foo = Foo { num: 1 };
        assert!(foo.num == 1);
        foo.num = 2;
        assert!(foo.num == 2);
        {
            let bar = &mut foo;
            bar.num += 1;
            assert!(bar.num == 3);
        }
        foo.num += 1;
        assert!(foo.num == 4);
    });

    let expected = format_js(concat!(
        r#"class Foo {
            constructor(num) {
                this.num = num;
            }
        }
        let foo = new Foo(1);
        console.assert(foo.num === 1);
        foo.num = 2;
        console.assert(foo.num === 2);
        {
            let bar = foo;
            bar.num += 1;
            console.assert(bar.num === 3);
        }
        foo.num += 1;
        console.assert(foo.num === 4);
        "#
    ));
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore = "TODO need solution for taking &mut field like let num = &mut foo.num;"]
#[tokio::test]
async fn mutate_mut_ref_of_non_copy_structs_primative_field() {
    let actual = r2j_block_with_prelude!({
        struct Foo {
            num: i32,
        }
        let mut foo = Foo { num: 1 };
        assert!(foo.num == 1);
        let num = &mut foo.num;
        *num += 1;
        assert!(foo.num == 2);
        foo.num += 1;
        assert!(foo.num == 3);
    });

    // One possible solution is to replace
    // let num = &mut foo.num;
    // with
    // let num = &mut foo;
    // and record in the RustType info that the var/type actually refers to the `.num` field and so whenever the type is used we add `.num` accordingly, ie if we are assigning to the field we add `.num`, if we are just passing it to a fn we do nothing and just pass it as is.
    // Not sure what unintended consequences this might have.
    // * Will make the generated code more confusing because it will be harder to reconcile with the original Rust
    // * Passing around an object where we are actually expecting eg a number might be a problem but given JS doesn't care what types it recieves, it might not be a problem.
    // I don't think it will work eg in the case of a fn `fn wants_mut_int(num: &mut i32) {}`. We would need to pass the entire foo object, so the code inside the fn would need to add the `.num` when updating it. But this fn could also be used elsewhere where an actual mut int (ie RustInteger) is passed to it. Could potentially have different branches for each but this would be complicated to implement and not a very clean solution.

    // The obvious/naive solution is that any time we create a mut var of a struct, we must replace all primative field values with a mut wraper equivalent eg RustInteger.
    // The problem is most of the time this will be completely unnecessary since usually when mutating a struct we are simply assigning a new value to a field using eg `=` or `+=`, and there is no way to tell ahead of time if we take a mut ref of a field `&mut my_struct.my_field` without looking ahead in the code. In fact even if there is a &mut of a field taken, only the child values of that field need be wrapped in mutable wrappers like RustInteger, again it would be very wasteful and inefficient to wrap everything single value, just in case. Even worse, some instantiations might take a mut ref, but others won't so we would either have to use multiple different definitions or have values unnecessarily wrapped. I think we just need to add the wrapper at the point which the class/struct is instantiated.
    // Given that Rust doesn't have mutable static fields like JS, all values in a struct must always be explicity specified when instantiating the struct (bar using defaults but that's fine), which means in the equivalent JS we can always specify every value in the struct, which is means we don't need the wrappers in the class definition, we can just wrap the args passed to the struct instantiation - NO this only works if the var is assigned directly by a struct/class instantiation, whereas if the var is assigned by eg a function that contains the instantiation, we are back to the previous problem of needing a different function for each instantiation, which scales very badly if there is eg multiple layers functions to instantiate the object.
    // A possible solution is to instead instantiate the struct as normal without wrappers and *then* go through the object and replace the necessary values with wrapped values. This is not the optimal solution and means doing redundant work compared to if the object had just been correctly created with the wrappers in the first place... but is probably best bet for now.
    // Also if the &mut field/index is an object containing primatives, this is fine, it is only &mut fields/indexes that are directly primatives, which seems quite niche so just trying compile time erroring for now and see how far we can get and how many libraries we can port without it.
    // Also it is not just fields and structs, eg `let foo = [1, 2]; let one = &mut data[0];` has the same problem.
    // Can we, in cargo-js, store a mutable ref to eg the JsExpr of the instantiation, eg on it's RustType or ScopedVar, and then if we later find a mut ref of a field is taken, we can update the JsExpr accordingly? Even if we can't keep a mut ref, we can give the expression a UUID and store that on the RustType then when we find any field mut refs, record this in the global data, then do a subsequent pass over the whole data to update/apply the stored data to the corresponding JsExpr's.

    // Another solution is to encourage use of types like maybe `type JsString = &'staic str; type JsNumber = &i32;` and ensure these types cannot be made mutable, so they behave like actual JS types and force users to write code more idiomatic to JS and avoid mutating primatives.

    // We could of course just compile time error for *any* use of a mutable JS primative and force users to explicitly wrap primatives when they need to be mutable. This is nice because it creates an intuitive 1:1 mapping between the source Rust and generated JS and discourages mutating primatives which is not idiomatic in JS. The problem is this means Rust that is not explicitly written for cargo-js might not compile. It is arguably similarly effective to simply advise against use of mutable primatives in code written specifically for cargo-js, and maybe even output warnings from the compiler. Could also have two compile flags, one that dissallows mutable primatives which is for writing "idiomatic" cargo-js code, and another flag for compiling "third party" Rust code which does allow/handle mutable primatives.

    let expected = format_js(concat!(
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
            }
            let foo = new Foo(new RustInteger(1));
            console.assert(foo.num.inner === 1);
            let num = foo.num;
            num.inner += 1;
            console.assert(foo.num.inner === 2);
            foo.num.inner += 1;
            console.assert(foo.num.inner === 3);
        "#
    ));

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn mutate_mut_ref_of_non_copy_structs_struct_field() {
    let actual = r2j_block_with_prelude!({
        struct Bar {
            num: i32,
        }
        struct Foo {
            bar: Bar,
        }
        let mut foo = Foo {
            bar: Bar { num: 1 },
        };
        assert!(foo.bar.num == 1);
        let bar = &mut foo.bar;
        bar.num += 1;
        assert!(foo.bar.num == 2);
        foo.bar.num += 1;
        assert!(foo.bar.num == 3);
        // assert!(bar.num == 3);
    });

    let expected = format_js(concat!(
        r#"
            class Bar {
                constructor(num) {
                    this.num = num;
                }
            }
            class Foo {
                constructor(bar) {
                    this.bar = bar;
                }
            }
            let foo = new Foo(new Bar(1));
            console.assert(foo.bar.num === 1);
            let bar = foo.bar;
            bar.num += 1;
            console.assert(foo.bar.num === 2);
            foo.bar.num += 1;
            console.assert(foo.bar.num === 3);
        "#
    ));
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn mutate_copy_struct() {
    // We cannot reuse a moved (ie assigned to var, passed to fn) var, so don't need to worry about mutations affecting orginal variable
    let actual = r2j_block_with_prelude!({
        #[derive(Clone, Copy)]
        struct Foo {
            num: i32,
        }
        let mut foo = Foo { num: 1 };
        assert!(foo.num == 1);
        foo.num = 2;
        assert!(foo.num == 2);
        let bar = &mut foo;
        bar.num += 1;
        assert!(bar.num == 3);
        foo.num += 1;
        assert!(foo.num == 4);
        let mut baz = foo;
        baz.num += 1;
        assert!(baz.num == 5);
        assert!(foo.num == 4);
    });

    let expected = format_js(concat!(
        r#"
            class Foo {
                constructor(num) {
                    this.num = num;
                }
                
                copy() {
                    return JSON.parse(JSON.stringify(this));
                }
            }
            let foo = new Foo(1);
            console.assert(foo.num === 1);
            foo.num = 2;
            console.assert(foo.num === 2);
            let bar = foo;
            bar.num += 1;
            console.assert(bar.num === 3);
            foo.num += 1;
            console.assert(foo.num === 4);
            let baz = foo.copy();
            baz.num += 1;
            console.assert(baz.num === 5);
            console.assert(foo.num === 4);
        "#
    ));
    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
