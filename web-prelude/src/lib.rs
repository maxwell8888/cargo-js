// TODO this is duplicated from ravascript-core/src/prelude.rs so need to eventually remove that dupication

#[derive(Debug, Default)]
pub struct Div {}

#[derive(Debug, Default)]
pub struct Document {
    // pub body: AnyNode,
}
impl Document {
    // TODO given we need to keep the untyped `create_element`, it seems somewhat pointless having this and we may as well go straight to `create_element_div` etc??
    #[allow(unused_variables)]
    pub fn create_element(tag_name: &'static str) -> Div {
        Div {}
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
