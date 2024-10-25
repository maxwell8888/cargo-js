//  TODO we can't publish std to crates.io so seems like we need to use a different name?
use ravascript::replace_with;
use std::marker::PhantomData;

// TODO this is duplicated from ravascript-core/src/prelude.rs so need to eventually remove that dupication

pub trait Node {
    // TODO deprecate append_child() in favour of append_child2
    fn append_child<T: Node>(&self, _a_child: T) {}
    // fn add_event_listener(&self, _action: &str, _callback: impl Fn(Event)) {}
    // fn add_event_listener_async<F>(&self, _action: &str, _callback: impl Fn(Event) -> F)
    // where
    //     F: Future<Output = ()>,
    // {
    // }
    fn before<T: Node>(&self, _node: T) {}
}
// impl Node for AnyNode {}

#[allow(unused_variables)]
pub trait Element: Node {
    fn set_attribute(&self, _attr_name: &str, _attr_val: &str) {}
    // TODO wait for async trait fns to stabilise
    // async fn request_fullscreen(&self) {}
    fn request_fullscreen(&self) {}
    // fn append_child(&self, child: DomNode);
    // fn get_self() -> Self;

    // classList methodshttps://developer.mozilla.org/en-US/docs/Web/API/Element/classList
    /// `.classList.add()`
    #[replace_with(field)]
    fn class_list(&self) -> DOMTokenList {
        DOMTokenList
    }
}

pub trait HtmlElement: Element {
    /// NOTE returns None/undefined
    fn focus(&self) {}
}

#[derive(Debug, Default)]
pub struct HtmlAnyElement {}
impl Node for HtmlAnyElement {}
impl Element for HtmlAnyElement {
    // fn get_self() -> Self {
    //     HtmlDivElement {}
    // }
}
impl HtmlElement for HtmlAnyElement {}

#[derive(Debug, Default)]
pub struct HtmlDivElement {}
impl Node for HtmlDivElement {}
impl Element for HtmlDivElement {
    // fn get_self() -> Self {
    //     HtmlDivElement {}
    // }
}
impl HtmlElement for HtmlDivElement {}

#[derive(Debug, Default)]
pub struct HtmlCanvasElement {}
impl Node for HtmlCanvasElement {}
impl Element for HtmlCanvasElement {
    // fn get_self() -> Self {
    //     HtmlDivElement {}
    // }
}
impl HtmlElement for HtmlCanvasElement {}
impl HtmlCanvasElement {
    pub fn get_context(&self, context_type: &str) -> CanvasRenderingContextAny {
        CanvasRenderingContextAny
    }
}
#[derive(Debug, Default)]
pub struct CanvasRenderingContextAny;

#[derive(Debug, Default)]
pub struct H1 {}

#[derive(Debug, Default)]
pub struct H2 {}

// Should JS window/document be const, static, struct, etc? const works quite well but semantically doesn't make much sense because JS window/document is actually a global mutable var, not just some inlined const value. Could also simply have a `fn window() -> Window` and `fn document() -> Document` which is guaranteed to always return current window document, and then users can still create aditional windows/documents with `window.open()` etc.
// Or maybe a static Mutex would be better? a mut global var is more semantically correct... but we would have to call .lock() etc.
// I think a freestanding fn is probably actually more idiomatic for Rust. I can't think of any examples but I assume the Rust std lib provides similar access to a singleton handle for some platform thing, and that would be the return of a function, not a global.
// TODO find comparable example in rust std lib.
pub fn document() -> Document {
    Document {}
}

#[derive(Debug, Default)]
pub struct Document {
    // pub body: AnyNode,
}
impl Document {
    // TODO given we need to keep the untyped `create_element`, it seems somewhat pointless having this and we may as well go straight to `create_element_div` etc??
    // #[allow(unused_variables)]
    // pub fn create_element(tag_name: &'static str) -> Div {
    //     Div {}
    // }
    // TODO it would be preferable to only have create_element(), and have create_element_div as a (unsafe?) wrapper and for the compiler to be smart enough to inline the create_element() directly. Or even better use the html builder syntax like `Div::new().append_child()` etc?? Either way still need an untyped create_element() for custom elements?
    pub fn create_element_div(&self) -> HtmlDivElement {
        HtmlDivElement {}
    }
    pub fn create_element_heading1(&self) -> H1 {
        H1 {}
    }
    pub fn create_element_heading2(&self) -> H2 {
        H2 {}
    }
    pub fn create_element(&self, tag_name: &'static str) -> HtmlAnyElement {
        HtmlAnyElement {}
    }
}

// TODO derives Default because we use it in `catch!` so that we can return an error and allow the code to compile
#[derive(Default, Debug)]
pub struct SyntaxError {
    // pub message: &'static str,
    pub message: String,
}
#[derive(Debug)]
pub struct Number {}

#[derive(Debug)]
pub struct Map<K, V> {
    key: PhantomData<K>,
    value: PhantomData<V>,
}

// TODO To create a `Value` should we:
// 1. Have a fn eg `from_something` where we check we check the type of the something value.
// 2. Keep it as a random value and do the type checking each time we do a match - this is harder, more inefficient (we surely have to do a match if we are going to do anything with the value), and means if the original value is not valid, then we will error when doing a match rather than a conversion, which is potentially harder to the debug if the value is now far away from where it was originally parsed.
#[derive(Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(Map<String, Value>),
}
// impl<T> From<Value> for T {
//     fn from(value: Value) -> Self {
//         todo!()
//     }
// }

#[derive(Debug)]
pub struct Something;

// impl From<Something> for Value {
//     fn from(value: Something) -> Self {
//         unsafe { Value::Null }
//     }
// }
impl Something {
    /// This function is a noop in the transpiled JS, it's only purpose is for casting types in the Rust source code. This is only suitable for converting to primative types or user types with no impls, since the JS is a noop, we do not actually converting the JS object to a class or adding anything to the prototype, the value remains the original Object/Array/Bool/Number/String and so attempting to call methods on it which are only defined on specific user type/class (rather than methods that are already defined on all objects/primatives) would result in a not found error. To convert to a user type with impls we must use Object.setProtoTypeOf which is not a JS noop and will actually update the prototype of the value so that methods can be called.
    pub unsafe fn cast<T>(self) -> T {
        unimplemented!()
        // T::default()
    }

    pub fn to_value(self) -> Value {
        unimplemented!()
    }
}

// NOTE even though we don't need any of the actual code from `Deserialize` for `JSON.parse()` to a concrete type, the type we are casting to does need to be a "simple" type that can be constructed from JSON, so it should be a trivial requirement that users `derive(Derserialize)` and this way we have something to construct T from. Otherwise could use Default, or just leave it unimplemented.

// TODO stringify should require deriving serde?
// pub fn stringify(_object: impl JsonStringyArg) -> &'static str {
//     todo!()
// }

// "Unlike most global objects, JSON is not a constructor. You cannot use it with the new operator or invoke the JSON object as a function. All properties and methods of JSON are static (just like the Math object)."

// Can throw SyntaxError. So leave it up to users to add try catch? Optionally yes but also provide a wrapper in web-helpers that is just parse that returns a Result instead.
// Parsing directly to a struct doesn't make much sense since it is impossible for JS to directly create a `class` (and so has the methods etc we would expect) from parse. Only an object/map can be returned. Need to use Object.assign(new Foo, { a: 1 }) or Object.setPrototypeOf({ a: 1 }, Foo.prototype) to convert object to a "class".
// Whilst sometimes we might want to convert the result to a class, sometime we just want to use the object like a class/struct with no methods, eg just directly use it to construct dom, and save the overhead of converting to a class. How do we do this? We could just "cast" it to a struct type, but how do we prevent that struct having methods and therefore requiring Object.assign etc? The compiler can just check if the type has any impls? Or even better if this particular instance of the type uses any impls?
// So allow "casting" to a struct (how?)
// Whilst we do want to allow directly creating succint JS, the focus should be providing safe abstractions that are guaranteed not to have hidden errors. So always use reviver. Always Object.assign? No this is pointless if there is no methods. But add this as an optimisation later. Of course the only reliable way to do this is to have type safety across the frontend and backend, in which case we don't actually really need to be
// all silent errors are UB so should be in `unsafe {}`
// Do like serde_json:
// let json: Value = serde_json::from_str(text)?;
// let json: Foo = serde_json::from_str(text)?;
// let json: i32 = serde_json::from_str(text)?;
// let json: Option<i32> = serde_json::from_str(text)?;
// pub fn parse_result<T: Deserialize>(_text: &str) -> Result<T, SyntaxError> {
//     unimplemented!()
// }

// The return value can be an: "Object, Array, string, number, boolean, or null value corresponding to the given JSON text".
// "Unsafe"
// Will panic/throw a SyntaxError if the JSON cannot be parsed.
pub fn json_parse(_text: &str) -> Something {
    // TODO add an actual implementation here?
    Something
}

// pub unsafe fn parse_cast<T>(_text: &str) -> T {
//     unimplemented!()
// }

fn testing_out_stuff() {
    #[derive(Default)]
    struct Foo;
    let something = json_parse("some json");
    let value: Foo = unsafe { something.cast() };
    let value2: Foo = unsafe { json_parse("some json").cast() };
}

// pub fn parse_2<T>(_text: &str, _reviver: ) -> T {
//     unimplemented!()
// }

pub struct DOMTokenList;
impl DOMTokenList {
    pub fn add(&self, class_name: &str) {}
    pub fn remove(&self) {}
    pub fn replace(&self) {}
    pub fn toggle(&self, class_name: &str) {}
    pub fn contains(&self) {}
}

#[macro_export]
macro_rules! try_ {
    ($try_block:block) => {{
        $try_block
    }};
}

// Don't want to require `Default`???
// #[macro_export]
// macro_rules! catch {
//     ($err_ident:ident, $ErrType:ty, $catch_block:block) => {{
//         let $err_ident: $ErrType = <$ErrType>::default();
//         $catch_block
//     }};
// }

#[macro_export]
macro_rules! catch {
    ($err_ident:ident, $ErrType:ty, $catch_block:block) => {{
        // Given the catch block code can use the err var like in `try {} catch (err) {}` we need to ensure it is defined in the output Rust code
        let $err_ident: $ErrType = <$ErrType>::default();
        $catch_block
    }};
}
