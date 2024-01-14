/// Arrays are allowed to be:
/// `mut` since JS arrays are mutable
/// `&` since we can just reassign the array which actually gives us a mutable reference but that doesn't matter since Rust won't let us make any mutations
/// `&mut`
pub struct Array<T> {
    vector: Vec<T>,

    /// Reflects the number of elements in an array.
    pub length: i32,
}
impl<T> Array<T> {
    fn new() -> Array<T> {
        Array {
            vector: Vec::new(),
            length: 0,
        }
    }

    // Static methods

    /// Creates a new Array instance from an iterable or array-like object.
    fn from() {}

    /// Creates a new Array instance from an async iterable, iterable, or array-like object.
    fn fromAsync() {}

    /// Returns true if the argument is an array, or false otherwise.
    fn isArray() {}

    /// Creates a new Array instance with a variable number of arguments, regardless of number or type of the arguments.
    fn of() {}
}

pub trait ArrayExtensions<T> {
    // Instance methods

    /// Returns the array item at the given index. Accepts negative integers, which count back from the last item.
    fn at() {}

    /// Returns a new array that is the calling array joined with other array(s) and/or value(s).
    fn concat() {}

    /// Copies a sequence of array elements within an array.
    fn copyWithin() {}

    /// Returns a new array iterator object that contains the key/value pairs for each index in an array.
    fn entries() {}

    /// Returns true if every element in the calling array satisfies the testing function.
    fn every() {}

    /// Fills all the elements of an array from a start index to an end index with a static value.
    fn fill() {}

    /// Returns a new array containing all elements of the calling array for which the provided filtering function returns true.
    fn filter() {}

    /// Returns the value of the first element in the array that satisfies the provided testing function, or undefined if no appropriate element is found.
    fn find() {}

    /// Returns the index of the first element in the array that satisfies the provided testing function, or -1 if no appropriate element was found.
    fn findIndex() {}

    /// Returns the value of the last element in the array that satisfies the provided testing function, or undefined if no appropriate element is found.
    fn findLast() {}

    /// Returns the index of the last element in the array that satisfies the provided testing function, or -1 if no appropriate element was found.
    fn findLastIndex() {}

    /// Returns a new array with all sub-array elements concatenated into it recursively up to the specified depth.
    fn flat() {}

    /// Returns a new array formed by applying a given callback function to each element of the calling array, and then flattening the result by one level.
    fn flatMap() {}

    /// Calls a function for each element in the calling array.
    fn forEach() {}

    /// Determines whether the calling array contains a value, returning true or false as appropriate.
    fn includes() {}

    /// Returns the first (least) index at which a given element can be found in the calling array.
    fn indexOf() {}

    /// Joins all elements of an array into a string.
    fn join() {}

    /// Returns a new array iterator that contains the keys for each index in the calling array.
    fn keys() {}

    /// Returns the last (greatest) index at which a given element can be found in the calling array, or -1 if none is found.
    fn lastIndexOf() {}

    /// Returns a new array containing the results of invoking a function on every element in the calling array.
    ///
    /// equivalent to .iter().cloned() for `number` and `string`, equivalent to `iter_mut()` for objects and arrays? or `.iter().cloned()` the the arrays and objects are Rc<T>
    fn map() {}

    /// Removes the last element from an array and returns that element.
    fn pop() {}

    /// Adds one or more elements to the end of an array, and returns the new length of the array.
    fn push(&mut self, element: T) {}

    /// Executes a user-supplied "reducer" callback function on each element of the array (from left to right), to reduce it to a single value.
    fn reduce() {}

    /// Executes a user-supplied "reducer" callback function on each element of the array (from right to left), to reduce it to a single value.
    fn reduceRight() {}

    /// Reverses the order of the elements of an array in place. (First becomes the last, last becomes first.)
    fn reverse() {}

    /// Removes the first element from an array and returns that element.
    fn shift() {}

    ///Extracts a section of the calling array and returns a new array.
    fn slice() {}

    /// Returns true if at least one element in the calling array satisfies the provided testing function.
    fn some() {}

    /// Sorts the elements of an array in place and returns the array.
    fn sort() {}

    /// Adds and/or removes elements from an array.
    fn splice() {}

    /// lReturns a localized string representing the calling array and its elements. Overrides the Object.prototype.toLocaleString() method.
    fn toLocaleString() {}

    /// Returns a new array with the elements in reversed order, without modifying the original array.
    fn toReversed() {}

    /// Returns a new array with the elements sorted in ascending order, without modifying the original array.
    fn toSorted() {}

    /// Returns a new array with some elements removed and/or replaced at a given index, without modifying the original array.
    fn toSpliced() {}

    /// Returns a string representing the calling array and its elements. Overrides the Object.prototype.toString() method.
    fn toString() {}

    ///Adds one or more elements to the front of an array, and returns the new length of the array.
    fn unshift() {}

    ///Returns a new array iterator object that contains the values for each index in the array.
    fn values() {}

    /// Returns a new array with the element at the given index replaced with the given value, without modifying the original array.
    fn with() {}
}

// impl <T> ArrayExtensions<T> for &[] {}
macro_rules! impl_array_extensions_for_all_arrays {
    ($($N:expr),+) => {
        $( impl<T> ArrayExtensions<T> for [T; $N] {} )+
    };
}

impl_array_extensions_for_all_arrays!(
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
    27, 28, 29, 30, 31, 32
);

// TODO better for this to resolve to [] we can have this output something different from the actual transpiler, and the below needs to just return the correct type, ie Array.
#[macro_export]
macro_rules! a {
    () => {
        Array::new()
    };
    ($elem:expr; $n:expr) => {
        let array = Array::new();
        for i in 0..n {
            arrary.push(expr);
        }
        array
    };
    ($($x:expr),+ $(,)?) => {{
        let mut array = Array::new();
        $( array.push($x); )*
        array
    }};
}
