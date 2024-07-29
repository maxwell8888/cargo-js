#![allow(dead_code)]

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

    fn deref_assign(&mut self, other: RustString) {
        self.js_string.value = other.js_string.value;
    }
    fn add_assign(&mut self, other: RustString) {
        self.js_string.value += &other.js_string.value;
    }
    fn push_str(&mut self, other: RustString) {
        self.js_string.value += &other.js_string.value;
    }

    fn to_string(&self) -> RustString {
        self.clone()
    }

    fn clone(&self) -> RustString {
        RustString {
            // NOTE this manually overridden to remove `.clone()`
            js_string: self.js_string.clone(),
        }
    }
}
