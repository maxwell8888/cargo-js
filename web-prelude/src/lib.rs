// TODO this is duplicated from ravascript-core/src/prelude.rs so need to eventually remove that dupication

#[derive(Debug, Default)]
pub struct Div {}

#[derive(Debug, Default)]
pub struct Document {
    // pub body: AnyNode,
}
impl Document {
    // TODO given we need to keep the untyped `create_element`, it seems somewhat pointless having this and we may as well go straight to `create_element_div` etc??
    pub fn create_element(tag_name: &'static str) -> Div {
        Div {}
    }
}
