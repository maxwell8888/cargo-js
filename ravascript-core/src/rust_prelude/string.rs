use crate::prelude::{JsBoolean, JsString, JsStringTrait};

use super::RustBool;

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
struct RustString {
    js_string: JsString,
}
impl RustString {
    fn eq(&self, other: &RustString) -> RustBool {
        RustBool {
            js_boolean: JsBoolean(self.js_string == other.js_string),
        }
    }

    fn ne(&self, other: &RustString) -> RustBool {
        RustBool {
            js_boolean: JsBoolean(self.js_string != other.js_string),
        }
    }
}
