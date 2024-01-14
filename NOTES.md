## Goals

Remember that ultimately our goal is to be able able to write idiomatic Rust that transpiles to JS, not to wrangle every detail of the JS API into idiomatic Rust. Keep the low level stuff dirty, error prone, and undiomatic, then provide abstractions to get a nice DX.

Goals are a tradeoff between:
fully supporting all Rust features/types
adding overhead
transpile times

In order of priority:

### Have an extensive std library

which uses the API from Rust std or defacto std crates like chrono etc.

So:
developers don't have to go searching for third party tools
different RavaScript apps look more similar
efforts can be pooled to improve implementations

Whilst the default prelude/std of Ravascript will only inlcude what is directly available in JS eg `.map()`, etc. We will provide an equivalent of `std::*` eg `ravascript_std::*` which provides implementations for things like `.sum()` and other things familar/expected by Rust programmers.
Do we want to provide everything, eg allowing `arr.length += 1` which creates empty slots? No reason not to, the only downside is muddying the API for 99% of users who won't use this. Maybe have a normal std, then an extended version with all this JS junk that people shouldn't typically use unless these are doing some crazy performance stuff. Also the more JS equivalent functionality we provide the more likely we are to run into differences in behaves from Rust. Providing a slimmed down API helps avoid this.

I thought we wanted to use `Vec` without overhead? Maybe using `Array` is not such a big deal? Does significantly reduce the simplicity and immediate understandability of Ravascript though...
Also I don't think it would be possible to optimise away the overhead from `class Vec { fn push(arg) { return self.array.push(arg) } }`, but I think we would be doing the same thing even if we transpiled directly so it's moot, unless we add to the array prototype. Adding to the prototype means we could selectively only add the methods that actually get used, but we could do this when creating wrapper classes also. The problem with prototypes is we will get names clashes for js methods vs rust methods

### no or low overhead, zero cost abstractions

In two respects:  
bundle size - this is less strict, especially given a program written in Ravascript vs JavaScript will not be like for like due to the different styles of the languages, and Ravascript will (usually?) produce more verbose code.
"call chain" ie calling stuff directly vs being wrapped in an object. this is more strict, but not important in all cases so will be fine to break in general but should allow for removing the overhead when required.

### fast transpile

Instant for small apps, under 1 sec for the largest apps

### Readable JavaScript

As much as possible the output JavaScript should be idiomatic and resemble the orignal Rust without too much boiler plate

## Design

### camelCase, snake_case, etc

We could use snake_case in RS and not do any conversion, but that would require either using camelCase in Rust for web APIs like `.createElement()`, or only converting those names.
In which case it is in some ways easier to just convert everything to camelCase, and this way the generated JS is more idiomatic.
Or we could use camelCase in Rust totally avoid conversion, but I think most people won't like writing Rust with camelCase.
It does have the benefit or clearly differentiating RS from normal Rust.
Maybe case conversions could be optional and avoided when faster transpile times are wanted.
Would need to test how much time this saves.

### idiomatic Rust vs more flexible JS-like bastardisation of Rust

We can either have two distinct approaches to transpilation, or try to combine both. Ideally we want to mix these together rather than have them separate so eg if you only write js-like stuff then that is how it is transpiled, but if you add std stuff like `&mut i32`, then we add wrappers.

Incompatible with Rust

-   Making all structs and enums copy, and deciding that all all copies are actually mutable references, thereby giving us `Rc<RefCell<T>>` like behaviour without the verbosity.

Compatible with Rust but forces incomplete syntax

Compatible with Rust but unidiomatic and/or inefficient for Rust

-   using `&self` methods to mutate values thereby allowing multiple mutable references without needing to use `Rc<RefCell<T>>`
-   adding JS array methods like `.push()` to Rust arrays to allow more JS-like `let a = [1, 2]; a.push(3);`. This could work in Rust by calling `.into_vec()` in `.push()`
-   adding JS String methods like `.trim()` to Rust string literals to allow more JS-like `let a = " hello ".trim();`
-   using lints

Compatible with Rust but adds overhead to JS

-   use wrappers around `number` to allow mutable references

Rust idiomatic:

We can redefine our own std items like `Vec` and `vec![]` which can have useful documentation relevant to the JS implementation but internally simply wrap the original `std` types so that the code can still be compiled with Rust.
This is better than using `no_std` because will still want `std::Vec` which our `Vec` wraps so RavaScript can still be compile with rustc.
This also means we can make our `Vec` and `String` `Copy` to avoid verbose `.clone()` calls.
We can also overwrite items which do not make sense in JS with empty/noop implementations
Should `vec![]` transpile directly to `[]` in which case `Vec` methods would need to be added as prototypes to `[]` (or could possibly transpile methods to wrapper functions which take `this` as the first argument)?

For usage that we don't want to allow, we can return errors that can be written to the browser page, and also provide a custom linter.

Do we want to be able to transpile existing Rust code, in which case we need full syntax and std support, or only programs written for RavaScript?
The former would allow sharing code between eg native apps and web apps, server and browser code.
In this case we would need to use verbose syntax that isn't relevant to JS like `.iter().map().collect()`. We could provide helper functions like `.map()` which transpiles directly for JS and for Rust call eg `.iter_mut().map().collect()`.

JS like is will more easily produce code with no overhead, we simply ignore `&`, `&mut`, and `*`.

### std and no_std

### How to handle Rust concepts that are meaningless in JS

Like `Box`

### Comparing JavaScript Object mutability and copy behaviour with Rust

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

### Modules

#### Approach

#### Other approaches considered

Nested functions don't work because you can't access super, and can't pass it down as a var because it doesn't exist yet...
