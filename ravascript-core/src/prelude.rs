mod array;
pub use array::*;

mod js_string;
pub use js_string::*;

mod js_number;
pub use js_number::*;

mod js_boolean;
pub use js_boolean::*;

pub mod web {
    use std::{future::Future, ops::Index};

    use super::{JsNumber, JsNumberTrait};

    pub struct Math();
    impl Math {
        pub fn abs<T: JsNumberTrait>(x: JsNumber<T>) -> JsNumber<T> {
            x.abs()
        }
        pub fn exp<T: JsNumberTrait>(x: JsNumber<T>) -> JsNumber<T> {
            // TODO need to split JsNumber into int and float so we can impl `.exp()`
            x
        }
    }

    pub trait JsStringTrait: PartialEq + Sized {
        fn abs(self) -> Self;
    }

    #[derive(Debug, Default)]
    pub struct RegExp {}
    impl RegExp {
        pub fn new(_pattern: &str, _global: &str) -> RegExp {
            RegExp::default()
        }
    }

    pub trait JsPattern {}
    impl JsPattern for RegExp {}
    impl JsPattern for &str {}
    pub trait Replace {
        fn js_replace(&self, _pattern: impl JsPattern, _replace_with: &str) -> Self
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl Replace for &str {}

    #[derive(Clone, Copy, Debug, Default)]
    pub struct JsArray {
        pub length: i32,
    }
    impl JsArray {
        pub fn slice(self, _index: i32) -> JsArray {
            JsArray::default()
        }
    }
    impl Iterator for JsArray {
        type Item = &'static str;

        fn next(&mut self) -> Option<Self::Item> {
            None
        }
    }

    pub trait Slice {
        fn slice1(self, _index: i32) -> Self
        where
            Self: Sized,
        {
            todo!()
        }
        fn slice2(self, _index1: i32, _index2: i32) -> Self
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl Slice for &str {}

    pub trait Concat {
        fn concat(self, _arg: &str) -> &str
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl Concat for &str {}

    pub trait Includes {
        fn includes(&self, _pattern: &str) -> bool
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl Includes for &str {}

    pub trait Split {
        fn js_split(&self, _on: &str) -> JsArray
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl Split for &str {}

    pub trait CharAt {
        // TODO can we type this to just be a char?
        fn char_at(&self, _index: i32) -> &str
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl CharAt for &str {}

    pub trait Length {
        // TODO can we type this to just be a char?
        fn length(&self) -> i32
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl Length for &str {}

    #[derive(Debug, Default)]
    pub struct Response {
        pub body: ResponseBody,
    }
    impl Response {
        pub async fn json<T>(&self) -> T {
            // let value_any = Box::new(()) as Box<dyn Any>;
            // *value_any.downcast::<T>().unwrap()
            todo!()
        }
    }

    #[derive(Debug, Default)]
    pub struct ResponseBody {}
    impl ResponseBody {
        pub fn pipe_through(
            &self,
            _text_decoder_stream: TextDecoderStream,
        ) -> TransformStreamReadableSide {
            TransformStreamReadableSide {}
        }
    }

    #[derive(Debug, Default)]
    pub struct TransformStreamReadableSide {}
    impl TransformStreamReadableSide {
        pub fn get_reader(&self) -> Reader {
            Reader::default()
        }
    }

    #[derive(Debug, Default)]
    pub struct Reader {}
    impl Reader {
        pub async fn read<T>(&self) -> T {
            todo!()
        }
    }

    #[derive(Debug, Default)]
    pub struct TextDecoderStream {}
    impl TextDecoderStream {
        pub fn new() -> TextDecoderStream {
            TextDecoderStream::default()
        }
    }

    #[derive(Debug, Default)]
    pub struct ObjectEntries {}
    impl Entries for ObjectEntries {}

    #[derive(Clone, Copy, Debug, Default)]
    pub struct Headers {}
    impl Headers {
        pub fn new() -> Headers {
            Headers::default()
        }
        pub fn append(&self, _key: &str, _value: &str) {}
        pub fn entries(&self) -> ObjectEntries {
            todo!()
        }
    }

    #[derive(Clone, Copy, Debug, Default)]
    pub struct Action {}

    #[derive(Clone, Copy, Debug)]
    pub enum Method {
        Delete,
        Get,
        Post,
        Put,
    }
    impl Default for Method {
        fn default() -> Self {
            Method::Get
        }
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch#body
    pub struct FetchBodyStruct {}
    pub trait FetchBody {}
    impl FetchBody for &str {}
    // impl FetchBody for FetchBodyStruct {}

    #[derive(Clone, Copy, Debug, Default)]
    pub struct FetchOptions<B: FetchBody + 'static> {
        method: Option<Method>,
        headers: Option<Headers>,
        body: Option<B>,
    }
    impl<B: FetchBody + 'static> FetchOptions<B> {
        pub fn new() -> FetchOptions<B> {
            FetchOptions {
                method: None,
                headers: None,
                body: None,
            }
        }
        pub fn method(mut self, method: Method) -> FetchOptions<B> {
            self.method = Some(method);
            self
        }
        pub fn headers(mut self, headers: Headers) -> FetchOptions<B> {
            self.headers = Some(headers);
            self
        }
        pub fn body(mut self, body: B) -> FetchOptions<B> {
            self.body = Some(body);
            self
        }
    }

    // pub trait FetchBody {}
    // impl FetchBody for &str {}
    // // impl FetchBody for FetchBodyStruct {}

    // pub struct FetchOptions {
    //     pub body: &'static dyn FetchBody,
    // }
    // impl FetchOptions {
    //     pub fn body(mut self, body: impl 'static + FetchBody) -> FetchOptions {
    //         self.body = &body;
    //         self
    //     }
    // }

    // pub trait FetchBody {}
    // impl FetchBody for &str {}
    // // impl FetchBody for FetchBodyStruct {}

    // pub struct FetchOptions<B: FetchBody + 'static> {
    //     pub body: B,
    // }

    // impl<B: FetchBody + 'static> FetchOptions<B> {
    //     pub fn body(mut self, body: B) -> FetchOptions<B> {
    //         self.body = body;
    //         self
    //     }
    // }

    #[derive(Clone, Copy, Debug)]
    pub struct Url {}
    /// https://developer.mozilla.org/en-US/docs/Web/API/fetch
    /// Default options are marked with *
    /// const response = await fetch(url, {
    ///     method: "POST", // *GET, POST, PUT, DELETE, etc.
    ///     mode: "cors", // no-cors, *cors, same-origin
    ///     cache: "no-cache", // *default, no-cache, reload, force-cache, only-if-cached
    ///     credentials: "same-origin", // include, *same-origin, omit
    ///     headers: {
    ///     "Content-Type": "application/json",
    ///     // 'Content-Type': 'application/x-www-form-urlencoded',
    ///     },
    ///     redirect: "follow", // manual, *follow, error
    ///     referrerPolicy: "no-referrer", // no-referrer, *no-referrer-when-downgrade, origin, origin-when-cross-origin, same-origin, strict-origin, strict-origin-when-cross-origin, unsafe-url
    ///     body: JSON.stringify(data), // body data type must match "Content-Type" header
    /// });
    ///
    /// Could stay closer to JS and use fetch!() which can take 1 or 2 arguments, Or build an API similar to reqwest?
    /// We are trading off easy to generate and predictable generated JS, familar to JS devs vs cleaner more idiotmatic Rust familiar to Rust devs
    /// Go with JS API for now otherwise it could be complicated to track potentially mutations to items over time, even with an AST
    /// Possibly best to go with JS at least at first so we don't back ourselves into a corner. And it is good to give people more direct control over the JS being output
    /// Using fetch!() means we can check the paths at compile time and only pass strings as path (not Url("/path")). But probably don't want hard coded paths anyway, they should all be coming from the backend/axum. But again, how would that be transpiled to JS? they could be CONST's which get output as const's, that then get inlined when code is optimised? But the strings will be in the backend code, how do we make that accessible? There is a few hacks we could use for Strings, but might we want to pass more complex data? Well it won't be program data like items with types, it will always be static data like str's (String's?) or other JSON serializable structures.
    /// We are going to want to share types like structs with methods anyway because they can represented in both JS and Rust and backend and frontend might have some logic they want to share (though might need to avoid using methods on builtin types), and so if the type complexity if already being shared, also sharing the data doesn't seem like a big stretch? eg might want something like `Paths` with `Paths::HOME` and `Path::product(id: usize)`.
    // pub struct Fetch {}
    // pub async fn fetch(_form_action: Url) -> Response {
    /// T is response json data type
    pub async fn fetch(_path: &str) -> Response {
        Response { body: todo!() }
    }

    // Could use macro rules to allow writing a json like object, but the fields are checked by the macro, and it gets transpiled to an actual object, but we loose the discoverability of methods. I think we can simply make the transpiler smart enough to convert a builder pattern -> setting fields on empty object -> defining object inline. It can do this automatically for built in structs, but might be easier to require anotating the struct with an attribute for user structs?
    // macro_rules! fetch_options {
    //     ($field_name:ident) => {};
    // }

    // pub async fn fetch2(_form_action: Url, _options: FetchOptions) -> Response {
    pub async fn fetch2<B: FetchBody + 'static, R>(
        _path: &str,
        _options: FetchOptions<B>,
    ) -> Response {
        Response { body: todo!() }
    }

    #[macro_export]
    macro_rules! try_ {
        ($try_block:block) => {{
            $try_block
        }};
    }
    #[macro_export]
    macro_rules! catch {
        ($err_ident:ident, $ErrType:ty, $catch_block:block) => {{
            let $err_ident: $ErrType = <$ErrType>::default();
            $catch_block
        }};
    }

    #[derive(Default)]
    pub struct SyntaxError {
        pub message: &'static str,
    }
    #[derive(Debug, Default)]
    pub struct Json {}
    impl Json {
        pub fn stringify(_object: impl JsonStringyArg) -> &'static str {
            todo!()
        }
        pub fn parse<T>(_text: &str) -> T {
            todo!()
        }
    }

    pub struct JsError {}
    pub fn try_(try_: impl Fn(), catch: impl Fn(JsError)) {
        todo!()
    }

    #[derive(Debug, Default)]
    pub struct FormDataEntries {}
    impl FormDataEntries {
        pub fn new(_dom_node: AnyNode) -> FormData {
            FormData::default()
        }
    }

    #[derive(Debug, Default)]
    pub struct FormData {}
    impl FormData {
        pub fn new(_dom_node: AnyNode) -> FormData {
            FormData::default()
        }
        pub fn entries(&self) -> FormDataEntries {
            FormDataEntries::default()
        }
    }
    pub trait Entries {}
    impl Entries for FormDataEntries {}

    pub trait JsonStringyArg {}
    impl JsonStringyArg for Object {}
    impl JsonStringyArg for &str {}

    #[derive(Debug, Default)]
    pub struct Object {}
    impl Object {
        pub fn from_entries(_entries: impl Entries) -> Object {
            Object::default()
        }
    }
    impl Index<&str> for Object {
        type Output = &'static str;

        fn index(&self, _key: &str) -> &Self::Output {
            &""
        }
    }

    #[derive(Debug)]
    pub struct Event {
        pub target: AnyNode,
    }
    impl Event {
        pub fn prevent_default(&self) {}
    }
    #[derive(Clone, Copy, Debug, Default)]
    pub struct ClassList {}
    impl ClassList {
        pub fn add(&self, _class_name: &str) {}
    }

    #[derive(Debug)]
    pub struct Console {}
    impl Console {
        pub fn log<T>(_to_log: T) {}
        pub fn assert(assertion: bool) {
            assert!(assertion);
        }
    }

    #[derive(Debug)]
    pub struct SseEvent {
        pub data: &'static str,
    }

    #[derive(Debug)]
    pub struct SseOptions {
        pub headers: Object,
        pub payload: &'static str,
    }

    #[derive(Debug)]
    pub struct Sse {}
    impl Sse {
        pub fn new(url: &str, options: SseOptions) -> Sse {
            todo!()
        }
        pub fn add_event_listener(&self, event_name: &str, callback: impl FnMut(SseEvent)) {
            todo!()
        }
    }

    #[derive(Clone, Copy, Debug, Default)]
    pub struct AnyNode {
        pub text_content: &'static str,
        pub class_list: ClassList,

        // TODO restrict these to Form dom nodes
        pub method: Method,
        // pub action: Action,
        // pub action: Url,
        pub action: &'static str,
        pub parent_element: Option<&'static AnyNode>,
    }

    pub trait Node {
        // TODO deprecate append_child() in favour of append_child2
        fn append_child3(&self, _child: AnyNode) {}
        fn append_child<T: Node>(&self, _a_child: T) {}
        fn add_event_listener(&self, _action: &str, _callback: impl Fn(Event)) {}
        fn add_event_listener_async<F>(&self, _action: &str, _callback: impl Fn(Event) -> F)
        where
            F: Future<Output = ()>,
        {
        }
        fn before<T: Node>(&self, _node: T) {}
    }
    impl Node for AnyNode {}

    pub trait Element: Node {
        fn set_attribute(&self, _attr_name: &str, _attr_val: &str) {}
        // TODO wait for async trait fns to stabilise
        // async fn request_fullscreen(&self) {}
        fn request_fullscreen(&self) {}
        // fn append_child(&self, child: DomNode);
        fn get_self() -> Self;
    }

    #[derive(Debug)]
    pub struct AnyElement {}
    impl Node for AnyElement {}
    impl Element for AnyElement {
        fn get_self() -> Self {
            AnyElement {}
        }
    }
    pub trait HtmlElement: Element {}
    pub struct AnyHtmlElement {}
    impl Node for AnyHtmlElement {}
    impl Element for AnyHtmlElement {
        fn get_self() -> Self {
            AnyHtmlElement {}
        }
    }
    impl HtmlElement for AnyHtmlElement {}

    pub struct Text {}
    impl Node for Text {}

    // pub trait DomNodeTrait {}
    // impl<T: DomNodeTrait> DomNodeTrait for Vec<T> {}

    pub struct Body {}
    impl Node for Body {}

    pub struct HTMLDivElement {}
    impl Node for HTMLDivElement {}
    impl Element for HTMLDivElement {
        fn get_self() -> Self {
            HTMLDivElement {}
        }
    }
    impl HtmlElement for HTMLDivElement {}

    pub struct Textarea {}
    impl Node for Textarea {}
    impl Element for Textarea {
        fn get_self() -> Self {
            Textarea {}
        }
    }
    impl HtmlElement for Textarea {}

    pub struct HTMLInputElement {
        pub value: &'static str,
    }
    impl Node for HTMLInputElement {}
    impl Element for HTMLInputElement {
        fn get_self() -> Self {
            HTMLInputElement { value: "" }
        }
    }
    impl HtmlElement for HTMLInputElement {}

    pub struct Date {
        pub iso_string: &'static str,
    }
    impl Date {
        pub fn from_iso_string(iso_string: &'static str) -> Date {
            Date { iso_string }
        }
    }

    pub struct Clipboard {}
    impl Clipboard {
        pub async fn write_text(&self, _new_clip_text: &str) {}
        pub async fn read_text(&self) -> &'static str {
            ""
        }
    }
    pub const NAVIGATOR: Navigator = Navigator {
        clipboard: Clipboard {},
    };
    // TODO how to force using NAVIGATOR not Navigator? could make clipboard private but then would need to convert clipboard() to clipboard
    pub struct Navigator {
        pub clipboard: Clipboard,
    }
    impl Navigator {
        pub const CLIPBOARD: Clipboard = Clipboard {};
    }
    // TODO it might be more ideal/idiomatic to implement like below, but would require more sohphisticated camel case handling
    // pub mod Navigator {
    //     pub struct Clipboard {}
    // }

    #[derive(Debug, Default)]
    pub struct Document {
        pub body: AnyNode,
    }
    impl Document {
        pub const DOCUMENT_ELEMENT: AnyElement = AnyElement {};
        // TODO need to convert these to actual booleans in the output JS
        // pub const FULLSCREEN_ELEMENT: Option<AnyElement> = None;
        pub const FULLSCREEN_ELEMENT: bool = false;

        /// TODO should return Option<DomNode> (except for Body) since js can null is there is no match
        pub fn query_selector(_selector: &str) -> AnyNode {
            AnyNode::default()
        }
        pub fn query_selector_body() -> Body {
            Body {}
        }
        pub fn query_selector2<T: Element>(_selector: &str) -> T {
            T::get_self()
        }
        pub fn get_element_by_id(_id: &str) -> AnyNode {
            AnyNode::default()
        }
        /// For type safety prefer to use `create_element_<tag name>`
        pub fn create_element(_tag: &str) -> impl HtmlElement {
            AnyHtmlElement {}
        }
        // pub fn create_element_div() -> HTMLDivElement {
        //     HTMLDivElement {}
        // }
        pub fn create_element_textarea() -> Textarea {
            Textarea {}
        }
        pub fn create_element2<T: Element>(_tag: &str) -> T {
            T::get_self()
        }
        pub fn create_text_node(_text: impl ToString) -> Text {
            Text {}
        }

        pub fn request_fullscreen() -> AnyNode {
            AnyNode::default()
        }
        pub const EXIT_FULLSCREEN: bool = false;
        pub fn exit_fullscreen() -> AnyNode {
            AnyNode::default()
        }
    }

    pub struct Timer {}
    pub fn set_interval(_callback: impl Fn(), _inc: i32) -> Timer {
        Timer {}
    }
    pub fn clear_interval(_timer: Timer) {}
}

mod typed {
    use super::web::*;
    impl Document {
        // TODO this doesn't seem to affect the visibility, think I need to use a feature flag or trait
        pub fn create_element_div() -> HTMLDivElement {
            HTMLDivElement {}
        }
    }
}

#[derive(Debug)]
pub struct Element {}
