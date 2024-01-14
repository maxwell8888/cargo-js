## Design

### How to handle Rust concepts that are meaningless in JS

Like `Box`

### Comparing JavaScript mutability and copy behaviour with Rust

In JS, strings are a primitive which is copied, so we implement `Copy` for `JsString`.
Technically JS strings can also be objects but they don't share the same mutable behaviour as other objects. https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#string_primitives_and_string_objects

We need to forbid implementing `Copy` on RavaScript items which gets transpiled to JS objects, because JS reassignment will always create a new mutable reference reference, similar to calling clone on `Rc<RefCell<T>>`.
`Copy` is fine for items that get transpiled to JS primitives like `string` and `number` because they are also copied in JS.
To have the behaviour of Rust's copy in JS we would need to add a `.copy()` method to all JS objects.
We could add it to all reassignments but this would mean also adding it on non-copy types that are actually just moves rather than copies, so copying the data would be unnecessary, and not insignificant, work.
Given JS objects behave like `Rc<RefCell<T>>`, we can allow the following behaviour for structs and enums (what about closures?)
It probably does makes sense to impl `Clone` on items because even though

#### Move

if the item is moved in Rust we can no long access the original variable so it does not matter that it is a mutable reference in JS because Rust will prevent us from using the original.

#### Immutable Reference

If we take an immutable reference of an item, when transpiling we ignore `&` and `*` and, similarly to moving, whilst in JS will then have mutable references to the original item, Rust will prevent us from mutating either the original or references.

#### Mutable Reference

If we take a mutable reference of an items, we again ignore the `&mut` and `*` when transpiling, and in JS will have a single mutable reference which we can mutate and will behave the same way as Rust

#### Rc<T>

Using `Rc` can be useful to avoid dealing with lifetime issues when using `&`.
Without `RefCell`, we a similar situation as immutable references, ie in JS we will have many mutable references but Rust prevents us from mutating them.
The difference is that for `Rc` we will have `.clone()` and `Rc::clone()` calls which we must ignore, but this would prevent us from using `Clone` and `.clone()` anywhere, without knowing items types and therefore which items are `Rc` so we can only ignore `.clone()` on those.
Note: it is necessary to provide some kind of cloning/deep copying, implemented in JS with JSON.stringify/parse, and `Clone` would be the most idiomatic and familiar thing in Rust.

#### Rc<RefCell<T>>

I believe we could just also ignore `.borrow_mut()` and then we will will have multiple mutable references in JS which correctly matches the behaviour of `Rc<RefCell<T>>`.
