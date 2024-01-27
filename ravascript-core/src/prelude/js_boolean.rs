pub struct JsBoolean(pub bool);
impl PartialEq for JsBoolean {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
