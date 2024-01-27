use crate::prelude::{JsBoolean, JsStringTrait};

/// ```js
/// class RustString {
///     constructor(jsString) {
///         this.jsString = jsString;
///     }
///     eq(other) {
///         return this.jsString === other.jsString;
///     }
///     ne(other) {
///         return this.jsString !== other.jsString;
///     }
/// }
/// ```
pub struct RustBool {
    pub js_boolean: JsBoolean,
}
impl RustBool {
    fn eq(&self, other: &RustBool) -> RustBool {
        RustBool {
            js_boolean: JsBoolean(self.js_boolean == other.js_boolean),
        }
    }

    fn ne(&self, other: &RustBool) -> RustBool {
        RustBool {
            js_boolean: JsBoolean(self.js_boolean != other.js_boolean),
        }
    }
    fn bool_and(&self, other: &RustBool) -> RustBool {
        RustBool {
            js_boolean: JsBoolean(self.js_boolean.0 && other.js_boolean.0),
        }
    }
}
