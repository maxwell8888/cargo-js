#![allow(non_snake_case)]

#[derive(Clone, Copy)]
pub struct JsString {
    /// Reflects the length of the string. Read-only.
    pub length: u32,
}

pub trait StrExtensions {
    //    Static methods
    /// Returns a string created by using the specified sequence of Unicode values.
    fn fromCharCode() {}

    /// Returns a string created by using the specified sequence of code points.
    fn fromCodePoint() {}

    /// Returns a string created from a raw template string.
    fn raw() {}

    // Instance methods
    /// Returns the character (exactly one UTF-16 code unit) at the specified index. Accepts negative integers, which count back from the last string character.
    fn at() {}

    /// Returns the character (exactly one UTF-16 code unit) at the specified index.
    fn charAt() {}

    /// Returns a number that is the UTF-16 code unit value at the given index.
    fn charCodeAt() {}

    /// Returns a nonnegative integer Number that is the code point value of the UTF-16 encoded code point starting at the specified pos.
    fn codePointAt() {}

    /// Combines the text of two (or more) strings and returns a new string.
    fn concat() {}

    /// Determines whether a string ends with the characters of the string searchString.
    fn endsWith() {}

    /// Determines whether the calling string contains searchString.
    fn includes() {}

    /// Returns the index within the calling String object of the first occurrence of searchValue, or -1 if not found.
    fn indexOf() {}

    /// Returns a boolean indicating whether this string contains any lone surrogates.
    fn isWellFormed() {}

    /// Returns the index within the calling String object of the last occurrence of searchValue, or -1 if not found.
    fn lastIndexOf() {}

    /// Returns a number indicating whether the reference string compareString comes before, after, or is equivalent to the given string in sort order.
    fn localeCompare() {}

    // TODO replace match_ with match
    /// Used to match regular expression regexp against a string.
    fn match_() {}

    /// Returns an iterator of all regexp's matches.
    fn matchAll() {}

    /// Returns the Unicode Normalization Form of the calling string value.
    fn normalize() {}

    /// Pads the current string from the end with a given string and returns a new string of the length targetLength.
    fn padEnd() {}

    /// Pads the current string from the start with a given string and returns a new string of the length targetLength.
    fn padStart() {}

    /// Returns a string consisting of the elements of the object repeated count times.
    fn repeat() {}

    /// Used to replace occurrences of searchFor using replaceWith. searchFor may be a string or Regular Expression, and replaceWith may be a string or function.
    fn replace() {}

    ///lUsed to replace all occurrences of searchFor using replaceWith. searchFor may be a string or Regular Expression, and replaceWith may be a string or function.
    fn replaceAll() {}

    /// Search for a match between a regular expression regexp and the calling string.
    fn search() {}

    /// Extracts a section of a string and returns a new string.
    fn slice() {}

    /// Returns an array of strings populated by splitting the calling string at occurrences of the substring sep.
    fn split() {}

    /// Determines whether the calling string begins with the characters of string searchString.
    fn startsWith() {}

    /// Returns a portion of the string, starting at the specified index and extending for a given number of characters afterwards.
    /// Deprecated
    fn substr() {}

    /// Returns a new string containing characters of the calling string from (or between) the specified index (or indices).
    fn substring() {}

    /// The characters within a string are converted to lowercase while respecting the current locale.
    ///
    /// For most languages, this will return the same as toLowerCase().
    fn toLocaleLowerCase() {}

    /// The characters within a string are converted to uppercase while respecting the current locale.
    ///
    /// For most languages, this will return the same as toUpperCase().
    fn toLocaleUpperCase() {}

    /// Returns the calling string value converted to lowercase.
    fn toLowerCase() {}

    /// Returns a string representing the specified object. Overrides the Object.prototype.toString() method.
    fn toString() {}

    /// Returns the calling string value converted to uppercase.
    fn to_upper_case(self) -> JsString
    where
        Self: Sized,
    {
        JsString { length: 0 }
    }

    /// Returns a string where all lone surrogates of this string are replaced with the Unicode replacement character U+FFFD.
    fn toWellFormed() {}

    /// Trims whitespace from the beginning and end of the string.
    fn trim() {}

    /// Trims whitespace from the end of the string.
    fn trimEnd() {}

    /// Trims whitespace from the beginning of the string.
    fn trimStart() {}

    /// Returns the primitive value of the specified object. Overrides the Object.prototype.valueOf() method.
    fn valueOf() {}
}
impl StrExtensions for &str {}
