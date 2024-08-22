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

pub trait Element: Node {
    fn set_attribute(&self, _attr_name: &str, _attr_val: &str) {}
    // TODO wait for async trait fns to stabilise
    // async fn request_fullscreen(&self) {}
    fn request_fullscreen(&self) {}
    // fn append_child(&self, child: DomNode);
    // fn get_self() -> Self;
}

pub trait HtmlElement: Element {}

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
    pub fn create_element_div(&self) -> HtmlDivElement {
        HtmlDivElement {}
    }
    pub fn create_element_heading1(&self) -> H1 {
        H1 {}
    }
    pub fn create_element_heading2(&self) -> H2 {
        H2 {}
    }
}

#[derive(Default)]
pub struct SyntaxError {
    // pub message: &'static str,
    pub message: String,
}
#[derive(Debug, Default)]
pub struct Json {}
impl Json {
    // TODO stringify should require deriving serde?
    // pub fn stringify(_object: impl JsonStringyArg) -> &'static str {
    //     todo!()
    // }
    pub fn parse<T>(_text: &str) -> T {
        todo!()
    }
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
