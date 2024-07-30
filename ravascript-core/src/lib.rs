use biome_formatter::{IndentStyle, IndentWidth};
use biome_js_formatter::{context::JsFormatOptions, JsFormatLanguage};
use biome_js_parser::JsParserOptions;
use biome_js_syntax::JsFileSource;

use handle_syn::{
    handle_item_const, handle_item_enum, handle_item_fn, handle_item_impl, handle_item_struct,
    handle_item_trait, handle_stmt, resolve_path, JsImplItem,
};
use js_ast::{
    FmtExtensions, Ident, JsClass, JsExpr, JsFn, JsLocal, JsModule, LocalName, LocalType, PathIdent,
};
use std::{fmt::Debug, fs, path::PathBuf};
use syn::{
    ExprPath, GenericParam, ImplItem, Item, ItemFn, ItemImpl, ItemMod, ItemTrait, PathArguments,
    Type, UseTree,
};
use tracing::debug_span;

mod handle_syn;
mod js_ast;
pub mod prelude;
pub mod rust_prelude;

mod duplicate_namespacing;
use duplicate_namespacing::{namespace_duplicates, Duplicate};

mod extract_modules;
use extract_modules::{extract_modules, ModuleDataFirstPass};

mod make_item_definitions;
use make_item_definitions::make_item_definitions;

mod update_item_definitions;
use update_item_definitions::{
    update_item_definitions, FnInfo, ItemDefinition, ModuleData, RustTraitDefinition,
    StructOrEnumDefitionInfo,
};

pub use js_ast::JsStmt;

const PRELUDE_MODULE_PATH: &str = "prelude_special_case";

// TODO need to handle expressions which return `()`. Probably use `undefined` for `()` since that is what eg console.log();, var x = 5;, etc returns;
// TODO preserve new lines so generated js is more readable
// TODO consider how to get RA/cargo check to analyze rust inputs in `testing/`

// TODO remove this as it is unnecessary redirection
/// Converts a Vec<syn::Item> to Vec<JsStmt> and moves method impls into their class
///
/// all users (eg crate, fn, file) want to group classes, but only crates want to populate boilerplate
fn js_stmts_from_syn_items(
    items: Vec<Item>,
    // Need to keep track of which module we are currently in, for constructing the boilerplate
    current_module: &[String],
    global_data: &mut GlobalData,
) -> Vec<JsStmt> {
    let span = debug_span!("js_stmts_from_syn_items", current_module = ?current_module);
    let _guard = span.enter();

    let mut js_stmts = Vec::new();
    // let mut modules = Vec::new();
    // TODO this should be optional/configurable, might not always want it

    // We need to know what the syn type is to know whether it is eg a use which needs adding to the boiler plate
    // but we also need to put the struct impls into the class
    // solution:
    // push use to boilerplate in handle_item() and don't push the item to js_stmts
    // push other items as normal
    // after loop, group together classes
    // now that classes are grouped, we can add them to the module object - we either do the conversion to module objects right at the end after parsing all branches, otherwise we are always going to have to be able to amend the classes because impls might be in other modules.

    // remember that `impl Foo` can appear before `struct Foo {}` so classes definitely need multiple passes or to init class when we come across an impl, and then place it and add other data when we reach the actual struct definition
    // What happens when a method impl is outside the class's module? Could just find the original class and add it, but what if the method if using items from *it's* module? Need to replace the usual `this.someItem` with eg `super.someItem` or `subModule.someItem`. So we need to be able to find classes that appear in other modules
    // dbg!("js_stmts_from_syn_items");
    // dbg!(&global_data.scope_id);
    for item in items {
        // handle_item(item, global_data, current_module, &mut js_stmts);
        match item {
            Item::Const(item_const) => {
                js_stmts.push(handle_item_const(
                    &item_const,
                    true,
                    global_data,
                    current_module,
                ));
            }
            Item::Enum(item_enum) => {
                js_stmts.push(handle_item_enum(
                    item_enum,
                    true,
                    global_data,
                    current_module,
                ));
            }
            Item::ExternCrate(_) => todo!(),
            Item::Fn(item_fn) => {
                js_stmts.push(handle_item_fn(&item_fn, true, global_data, current_module));
            }
            Item::ForeignMod(_) => todo!(),
            Item::Impl(item_impl) => {
                // TODO maybe it would be better for handle_item_impl (and similar fns) to return a JsClass and then we wrap it into a stmt here?
                js_stmts.extend(handle_item_impl(
                    &item_impl,
                    true,
                    global_data,
                    current_module,
                ));
            }
            Item::Macro(_) => todo!(),
            Item::Mod(_item_mod) => {
                // NOTE in contrast to the other handlers here, handle_item_mod actually mutates `current_module_path` and appends a new JsModule to `global_data.transpiled_modules` instead of appending statements to `js_stmts`
                // handle_item_mod(item_mod, global_data, current_module)
            }
            Item::Static(_) => todo!(),
            Item::Struct(item_struct) => {
                let js_stmt = handle_item_struct(&item_struct, true, global_data, current_module);
                js_stmts.push(js_stmt);
            }
            Item::Trait(item_trait) => {
                handle_item_trait(&item_trait, true, global_data, current_module);
                js_stmts.push(JsStmt::Expr(JsExpr::Vanish, false));
            }
            Item::TraitAlias(_) => todo!(),
            Item::Type(_) => todo!(),
            Item::Union(_) => todo!(),
            Item::Use(_item_use) => {
                //
                // handle_item_use(&item_use, ItemUseModuleOrScope::Module(module));
            }

            Item::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }

    js_stmts
}

#[allow(dead_code)]
#[derive(Default, Clone, Debug)]
struct RustPreludeTypes {
    vec: bool,
    hash_map: bool,
    // TODO need to check which fns return an Option, and set `option` for those
    option: bool,
    some: bool,
    // TODO Is this just null?
    none: bool,
    result: bool,
    ok: bool,
    err: bool,
    assert_eq: bool,
    assert_ne: bool,
    dbg: bool,
    // println: bool,
    // print: bool,
    number_prototype_extensions: bool,
    string_prototype_extensions: bool,
    boolean_prototype_extensions: bool,
    integer: bool,
    float: bool,
    string: bool,
    bool: bool,
    equals: bool,

    // New approach
    // Basic `RustInteger`
    rust_integer: bool,
    rust_string: bool,
    rust_array_copy: bool,
    option_is_some_and: bool,
    option_unwrap: bool,
    result_is_err: bool,
    result_unwrap: bool,
}

// Third party crates
#[derive(Debug, Clone)]
struct CrateData {
    _name: String,
    // Ideally we would just store the data like this, but we need to be able to resolve third party crate use statements, which might chain use statements, using `get_path_without_namespacing` just like any other module, so we need to maintain the same data structure? Yes we need to parse the third party crate anyway since we need to include it's source the JS output so will already have all it's ModuleData. Although in theory we could just do a one off calculation of all it's crate level pub module paths/items and only look for those when resolving paths in the main crate, which would reduce work, for now we will just resolve the paths just like any other module
    // (name, module path, definition)
    // items: Vec<(String, Vec<String>, ItemDefinition)>,
    _modules: Vec<ModuleData>,
}

/// Types are ultimately needed for:
/// * We can properly cast eg Json::stringify() to JSON.stringify(). I think it most cases we can correctly handle these cases and differentiate from user types by looking up the import, but if the user does something like `let other = Json; other::stringify_with_args()` (not actually valid, can't assign a struct to a var, but can't think of any proper examples right now) or something, we need to know the type of other to know if it's methods need renaming???
/// * calling associated functions on a generic. generics are just types so can't have . methods, but they can have :: asociated fns. We need to know the concrete/runtime type of the generic to know which type to use the `Foo` in `Foo::bar()`.
/// * When we have something that will transpile to a JS primitive so that we know:
///     * it is safe to use operators eg ===, +, etc in place of method equivalents eg .eq(), .add(), etc
///     * `mut`s and `&mut`s must be wrapped in an object
/// * How to deal with dereferencing (ie need to know if we have &mut or & ???)
/// * We can ignore `.clone()` on strings
/// * When a `Copy` item is copied, that will transpiled to an object or array in JS, we must add a `.copy()` or `.clone()` or something
/// * Able to error when attempting to transpile an unimplemented/unsupport method on eg `Option`.
/// * Allows only importing minimum required prelude. The full prelude will be large given the large amount of methods on Vec, Option, etc, so we do want to only import what is actually used, and to know what is actually used we can't just look for use of eg Vec, Some, we also need to look for eg `.map()` but is that map for a Vec, Option, or user defined? we can only know if we know the type of what it is being called on.
/// * `let num = 5;` (or more likely getting a number from JSON) doesn't tell you the type so we need to know what it is eventually inferred as to know whether to use bigint
///
/// "This also made me realise we might want to keep different number types in the transpiled JS, eg i32, f64, as they would have different methods?" I think this just means we need to use `someI32Method(5)` rather than `5.someI32Method()`
///
/// Types are therefore needed to:
///
/// Why not use `syn::Type`? No reason I think. It don't have variants for builtins like i32, but we probably will end up copying most of the variants to handle more complex type with bounds like `impl T + W` etc. Can't work out where is stores the generic for eg `Foo<Bar>`, the Path variant seems to just be a path... TODO debug to test this
///
/// I don't see how any of this requires a Enum or Struct variant/type?
///
/// Generics are probably the hardest part because they don't get defined till later, but by the time we are actually eg calling methods on the type, we should know the generic?
///
/// Do we need separate types for Option and Result?
///
// #[derive(Debug, Clone)]
// enum RustType {
//     // JustForReference(Type),
//     /// For cases/expressions we know cannot return a type, eg `break`
//     NotAllowed,
//     /// Can't be known at this point in analysis, eg the type is inferred somewhere else in the code
//     Unknown,
//     /// Needs implementing
//     Todo,
//     /// ()
//     Unit,
//     /// !
//     Never,
//     /// Shouldn't be used, just used to represent some like the `ToString` in `T: ToString` which should subsequently get ignored and eventually just returned as `RustType::Generic("T")`. What????
//     /// Fns might return impl FooTrait, and that will also be the type of eg any var that the result is assigned to. It's fine not knowing the exact type because you can only call the trait's methods on it, but need to be able to look up the trait's methods to know what type they return.
//     ImplTrait(Vec<InstanceTypeImplTrait>),
//     /// I think this is useful for eg within a fn with generic T and we have vars that have type T
//     /// (name, type)
//     TypeParam(RustTypeParam),
//     /// name
//     TypeParamSimple(String),
//     I32,
//     F32,
//     Bool,
//     String,
//     /// (generic)
//     Option(Box<RustType>),
//     /// (generic)
//     Result(Box<RustType>),
//     /// Need to remember that because StructOrEnum can have generics that have been resolved to concrete types, it means we are using RustType to record the type of both the single defined item that gets recorded in scopes modules/structs and enums, but also instances of the item, stored the in variables vec of the scope
//     Struct(StructOrEnumInstance),
//     Enum(StructOrEnumInstance),
//     Vec(Box<RustType>),
//     Array(Box<RustType>),
//     Tuple(Vec<RustType>),
//     /// (&mut T)
//     MutRef(Box<RustType>),
//     /// (& T) useful to track & as well as &mut so we know what * is operating on??
//     Ref(Box<RustType>),
//     /// (return type)
//     Fn(Box<RustType>),
// }

// Why do we need this? How is it different to RustType?
// MemberType is for types in item definitions and impls, RustType is for instance types
// One reason to have both is that it makes it easier to reason about, ie are we dealing with a definition or an instance
// MemberType `::ParentItem` because... we need some way of signaling it is the same type as the parent type, I guess we could just store the name?
// Most importantly we need to distinguish between whether eg `self` is returned or a new Foo, because in the former case we want to keep any type params that have been made concrete, and in the latter we want a fresh type? this is different from *Self* and Foo, and so named ParentItem instead of self. If we used the body to determine the return type, it would be easy.
// The main difference is that RustType Can be ::NotKnownYet, but this is actually very rare, numbers are the only case I can think of ie `let five = 5;`
// All this is why we can't just have a RustType::String(String), and need to store all the generics on it - exactly only the generics, don't need to store members etc?
// For MemberType, type parameters *cannot be resolved*, for RustType type parameters can be resolved to concrete types
//
// We separate out types like eg ::Vec because we want to know if the type is a js built in so we can convert [].push(x) to push([], x)
//
// Do also need to consider stuff like `impl X for Vec<T>` and `impl X for (i32, i32)`, ie we don't have a *user* type (ie struct/enum) to attach "members" to, so instead need to store a list of ItemType's that have members attached so when we have eg `(3, 3).my_foo()` we can lookup the return type of `.my_foo()` (and also know when to compile to eg myFoo([3, 3]))
//
// So ItemType is effectively used to point to the other types that appear in the item definiton.
//
// It makes sense to just use one of ItemType/InstanceType because they are practically the same and type instances will need to look up impls that match their type
// Also, for matching, Foo<i32> will need to match Foo<T>, etc so it is not as easy as doing `x == y`
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
enum RustType {
    /// For cases/expressions we know cannot return a type, eg `break`
    NotAllowed,
    /// Can't be known at this point in analysis, eg the type is inferred somewhere else in the code
    Unknown,
    // /// Unitialized/null
    // Uninit,
    /// Needs implementing
    Todo,
    // Self_,
    /// I think ParentItem means it is actually `self` not just `Self`???
    /// NOTE ParentItem is always self or Self, and these keywords are *always* referring to the target in an impl block, so if we come across a RustType::ParentItem we can determine what it is by looking up the global_data.impl_target or whatever it is called
    /// NOTE `Self` can also be used directly in a eg struct def like `struct Foo(Box<Self>);`. We are not currently handling/supporting these cases but need to bear this in mind for `RustType::ParentItem`
    /// NOTE if ParentItem is returned by an impl item fn it must be immediately converted to the receiver type so that we can be sure that we are in a static fn/def when parsing and we come across a ParentItem
    ParentItem,
    /// ()
    Unit,
    /// !
    Never,
    /// Fns might return impl FooTrait, and that will also be the type of eg any var that the result is assigned to. It's fine not knowing the exact type because you can only call the trait's methods on it, but need to be able to look up the trait's methods to know what type they return.
    ///
    /// Vec<(module path, scope id, RustTypeImplTrait (ie simple FooTrait or a fn trait: Fn(i32) -> i32))>
    ImplTrait(Vec<(Vec<String>, Option<Vec<usize>>, RustTypeImplTrait)>),
    /// Why does RustTypeParam need to hold resolved values, surely when the param is resolved we just use that type directly? In some cases, eg a fn that returns T, if T is resolved we can just return the resolved type when the fn is called. Other times it might be that the type is resolved, but we need to know later down the line which param was resolved so we can resolve the param where it is used in other places??? Possible but need examples to justify it.
    TypeParam(RustTypeParam),
    // TODO does TypeParam need to store information about the bounds?
    // TODO surely a struct and impl block can have multiple type params with the same name? So we need to keep track of where the param was defined to know which param eg a method arg's type should update
    // TypeParam(String),
    /// name
    // TypeParamSimple(String),
    I32,
    F32,
    Bool,
    String,
    /// (generic)
    /// The RustType for option *must* be a type param. (I believe) It is important to always have a type param to ensure we can always lookup the name of the generic to match against generics in methods etc NO, we can always lookup the name of the generic on the item_def
    Option(RustTypeParam),
    // Option(Box<RustType>),
    /// (generic)
    Result(RustTypeParam),
    /// Need to remember that because StructOrEnum can have generics that have been resolved to concrete types, it means we are using RustType to record the type of both the single defined item that gets recorded in scopes modules/structs and enums, but also instances of the item, stored the in variables vec of the scope
    // Struct(StructOrEnumItemDefinition),
    // Enum(StructOrEnumInstance),
    // Don't need to store all info about the type, because it should already be stored in the global scope as module data or scoped items. Just need to store a path to that item? Remember we are going to have to resovle paths to items any, ie if we find a `Foo` path we will need to follow the use stmts to find the module it is defined in. For ItemTypes yes, for InstanceTypes I think also yes but we need to all store the resolved state of the generics for each nested type
    // I think we do need to copy the data into the InstanceType because for scoped items we might have a Foo which then gets defined again in a lower scope so if we just look it up to get member info we will find the wrong one and have nothing to pin it to differentiate with like module paths. Yes but we could also give the item definitions indexes or unique ids and then we only have to store those with the InstanceType... NOTE in an item definition, while that items type params won't be resolved, the other types it uses in it's definition might be eg `stuct Foo { bar: Bar<i32> }`
    ///
    /// TODO storing a module path doesn't make much sense if the struct/enum/fn is scoped? Could use an Option which is None if the item is scoped? For now just store the Vec as whatever the current module is (even though this could be confusing for a scoped item), because it doesn't really matter since we always look for scoped items first, and determining whether eg handle_item_fn is for a module level fn or scoped fn would require passing extra args... NO actually we need to know whether we are top level or in a scope because currently we are putting all fns handled with handle_item_fn into the current scope, even if they are top level... which of course should just be scope=0, but this is not a nice approach
    /// TODO IMPORTANT we can't use the paths to definitions approach anyway because instances can exist in parent scopes of the item definition's scope. The best approach seems to be to simply store the item definition (or a reference to it) on the RustType as this seems to be how Rust itself models where/how item are allowed to be used/instantiated. eg:
    /// ```rust
    /// struct AmIHoisted {
    ///     ohno: String,
    /// }
    /// let cool = {
    ///     struct AmIHoisted {
    ///         ohno: i32,
    ///     }
    ///     let am_i_hoisted = AmIHoisted { ohno: 5 };
    ///     am_i_hoisted
    /// };
    /// assert!(cool.ohno == 5);
    /// ```
    /// Alternatively, this wouldn't be a problem if we hoisted *all* scoped definitions to the module level.
    ///
    /// (type params, module path, scope id, name)
    StructOrEnum(Vec<RustTypeParam>, Vec<String>, Option<Vec<usize>>, String),
    // Struct(Vec<RustTypeParam>, Vec<String>, String),
    /// (type params, module path, name)  
    // Enum(Vec<RustTypeParam>, Vec<String>, String),
    // TODO Should we use the same type for both Arrays and Vecs, because they get transpiled to the same thing anyway? NO because we need to handle the types differently, ie arrays need `.copy()` adding when they are moved (although this won't be necessary if the previous value is not used after the move/copy, but this would be hard to determine so need to just always add copy).
    Vec(Box<RustType>),
    Array(Box<RustType>),
    Tuple(Vec<RustType>),
    /// Even though Box::new() vanishes when transpiled, we need to keep track of which vars are Boxed because the dereferencing behaves differently
    Box(Box<RustType>),
    /// ie `type FooInt = Foo<i32>;`
    /// (name, type)
    UserType(String, Box<RustType>),
    /// (&mut T)
    MutRef(Box<RustType>),
    /// (& T) useful to track & as well as &mut so we know what * is operating on?? NO I think it doesn't matter in practice, we can just check if we have a `&mut` expr and if not just ignore the *
    Ref(Box<RustType>),
    /// (type params, return type)
    // Fn(Vec<RustTypeParam>, Box<RustType>),
    /// fn might be an associated fn in which case first arg will be Some() containing the (possibly resolved) generics of the impl target/self type. Possibly want to also record which type params are defined on the impl block, but see if we can get away without it initially given any impl block type params pretty much have to appear in the target/self type.
    /// (item type params, type params, module path, scope id, name)
    Fn(
        Option<Vec<RustTypeParam>>,
        Vec<RustTypeParam>,
        Vec<String>,
        Option<Vec<usize>>,
        // TODO arguably it would be better to just store the path and item name all in one, and when looking up the item/fn we are able to determine at that point whether the final one or two elements of the path are a item or associated fn or whatever
        RustTypeFnType,
    ),
    /// For things like Box::new where we want `Box::new(1)` -> `1`
    FnVanish,
    /// We need a separate type for closures because there is no definition with a path/ident to look up like RustType::Fn. Maybe another reason to store the type info directly and avoid using lookups so we don't need two separate variants.
    /// (input types, return type)
    Closure(Vec<RustType>, Box<RustType>),
}
impl RustType {
    fn is_js_primative(&self) -> bool {
        match self {
            RustType::NotAllowed => todo!(),
            RustType::Unknown => todo!(),
            RustType::Todo => todo!(),
            RustType::ParentItem => todo!(),
            RustType::Unit => todo!(),
            RustType::Never => todo!(),
            RustType::ImplTrait(_) => todo!(),
            RustType::TypeParam(_) => todo!(),
            RustType::I32 | RustType::F32 | RustType::Bool | RustType::String => true,
            RustType::Option(inner) => {
                //
                match &inner.type_ {
                    RustTypeParamValue::Unresolved => todo!(),
                    RustTypeParamValue::RustType(resolved_type) => resolved_type.is_js_primative(),
                }
            }
            RustType::Result(_) => todo!(),
            RustType::StructOrEnum(_, _, _, _) => false,
            RustType::Vec(_) => todo!(),
            RustType::Array(_) => false,
            RustType::Tuple(_) => todo!(),
            RustType::UserType(_, _) => todo!(),
            RustType::MutRef(_) => false,
            RustType::Ref(_) => todo!(),
            RustType::Fn(_, _, _, _, _) => false,
            RustType::Closure(_, _) => todo!(),
            RustType::FnVanish => todo!(),
            RustType::Box(_) => todo!(),
        }
    }
    fn is_mut_ref_of_js_primative(&self, impl_targets: &[RustType]) -> bool {
        match self {
            RustType::NotAllowed => todo!(),
            RustType::Unknown => todo!(),
            RustType::Todo => todo!(),
            RustType::ParentItem => {
                // TODO do we need to consider some kind of recursive
                impl_targets.last().unwrap().is_mut_ref_of_js_primative(&[])
            }
            RustType::Unit => false,
            RustType::Never => todo!(),
            RustType::ImplTrait(_) => todo!(),
            RustType::TypeParam(_) => todo!(),
            RustType::I32 => false,
            RustType::F32 => false,
            RustType::Bool => false,
            RustType::String => false,
            RustType::Option(_) => todo!(),
            RustType::Result(_) => todo!(),
            RustType::StructOrEnum(_, _, _, _) => false,
            RustType::Vec(_) => todo!(),
            RustType::Array(_) => todo!(),
            RustType::Tuple(_) => todo!(),
            RustType::UserType(_, _) => todo!(),
            RustType::MutRef(inner) => inner.is_js_primative(),
            RustType::Ref(_) => todo!(),
            RustType::Fn(_, _, _, _, _) => todo!(),
            RustType::Closure(_, _) => todo!(),
            RustType::FnVanish => todo!(),
            RustType::Box(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RustTypeFnType {
    /// (fn name)
    Standalone(String),
    /// (item name, fn name)
    AssociatedFn(String, String),
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
enum RustTypeImplTrait {
    SimpleTrait(String),
    /// (return type)
    Fn(RustType),
}

// Do we want to also store the trait bounds of each type param? This way if we have an unresolved type param that calls some function, we will know what trait to look up to find it. In some cases this might also remove the need for looking forward to resolve type params, if all we need to do with the type param/value/intance/type is call a method on it.
#[derive(Debug, Clone, PartialEq, Eq)]
struct RustTypeParam {
    name: String,
    type_: RustTypeParamValue,
}
// impl RustTypeGeneric

#[derive(Debug, Clone, PartialEq, Eq)]
enum RustTypeParamValue {
    /// Can't be known at this point in analysis, eg the type is inferred somewhere else in the code
    Unresolved,
    RustType(Box<RustType>),
}

#[derive(Debug, Clone)]
struct ScopedVar {
    name: String,
    mut_: bool,
    // TODO don't need this because it is part of the type and is record by the `type_` field
    // mut_ref: bool,
    // TODO
    type_: RustType,
}
impl ScopedVar {
    #[allow(dead_code)]
    fn is_mut_ref(&self) -> bool {
        matches!(self.type_, RustType::MutRef(_))
    }
}

// #[derive(Debug, Clone)]
// struct EnumInfo {
//     ident: String,
//     methods: Vec<ImplItemTemp>,
// }

// #[derive(Debug, Clone)]
// struct StructOrEnumMethod {
//     item_name: String,
//     return_type: RustType,
// }

// #[derive(Debug, Clone)]
// enum StructOrEnumSynObject {
//     Struct(ItemStruct),
//     Enum(ItemEnum),
// }

// #[derive(Debug, Clone)]
// enum MemberInfoFieldOrVariant {
//     Field,
//     Variant,
// }

#[allow(dead_code)]
#[derive(Debug, Clone)]
enum ItemDefintionImpls {
    /// For impls like `impl<T> Foo for T {}` where multiple types use the same impl so we transpile the impl block itself into a class, and point to this classes methods using eg `getFoo = impl__Foo__for__T.prototype.getFoo` or whatever.
    ///
    /// (module path, unique identifier/js_name?, method name)
    // GenericImpl(Option<Vec<String>>, String, String),
    /// I think we don't need a module path because we don't care what module the impl is in, as long as the target matches, we want to implement it
    /// (unique identifier/js_name?, method name)
    GenericImpl(String, String),
    ConcreteImpl(Vec<ImplItem>),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
enum ItemDefinitions {
    Struct,
    Enum,
    Fn,
}

/// Similar to ImplItemTemp but with less info (might turn out that we need to add more) because we are only using it for scoped items
// #[derive(Debug, Clone)]
// struct MethodInfo {
//     method_ident: String,
//     item_stmt: JsImplItem,
//     return_type: RustType,
// }

#[derive(Debug, Clone)]
struct RustGeneric {
    ident: String,
    // (module path, trait name)
    trait_bounds: Vec<(Vec<String>, Option<Vec<usize>>, String)>,
}

#[derive(Debug, Clone)]
struct RustImplBlockSimple {
    unique_id: String,
    // TODO Should this include generics that are defined on the target type, or just new generics introduced for the impl Trait or used in the methods/items? For now just assume it is everything.
    generics: Vec<RustGeneric>,
    trait_: Option<(Vec<String>, Option<Vec<usize>>, String)>,
    // Note this can a generic param
    target: RustType,
    rust_items: Vec<RustImplItemNoJs>,
    // items: Vec<ImplItem>,
}
fn get_item_impl_unique_id(
    module_path: &[String],
    scope_id: &Option<Vec<usize>>,
    item_impl: &ItemImpl,
) -> String {
    let params = item_impl
        .generics
        .params
        .iter()
        .map(|p| match p {
            GenericParam::Lifetime(_) => todo!(),
            GenericParam::Type(type_param) => type_param.ident.to_string(),
            GenericParam::Const(_) => todo!(),
        })
        .collect::<Vec<_>>()
        .join(",");
    let trait_ = match &item_impl.trait_ {
        Some((_, trait_, _)) => trait_
            .segments
            .iter()
            .map(|s| s.ident.to_string())
            .collect::<Vec<_>>()
            .join("::"),
        None => "".to_string(),
    };
    let target = match &*item_impl.self_ty {
        Type::Path(type_path) => type_path
            .path
            .segments
            .iter()
            .map(|s| s.ident.to_string())
            .collect::<Vec<_>>()
            .join("::"),
        _ => todo!(),
    };
    format!(
        "module path: {:?}, scope id: {:?}, type params: {params}, trait: {trait_}, target: {target}",
        module_path, scope_id
    )
}

#[derive(Debug, Clone)]
struct JsImplBlock2 {
    unique_id: String,
    _generics: Vec<RustGeneric>,
    trait_: Option<(Vec<String>, Option<Vec<usize>>, String)>,
    // Note this can a generic param
    target: RustType,
    /// Vec<(TODO whether the method actually gets used (for some eg `impl<T> Foo for T {}` blocks that apply to everything, it is hard to work out for which items the methods are actually used, since the criteria is whether the impl'd trait ie Foo is in scope/accessible at the point that the method is called, so the easiest approach is to just add everything and then track which methods actually get called), JS method/field)>
    items: Vec<(bool, RustImplItemJs)>,
}
impl JsImplBlock2 {
    fn js_name(&self) -> Ident {
        let trait_name = match &self.trait_ {
            Some((_module_path, _scope_id, name)) => name,
            None => "no_trait",
        };
        fn rust_type_js_name(rust_type: &RustType) -> String {
            match rust_type {
                RustType::StructOrEnum(_, _, _, name) => {
                    // TODO get proper deduplicated js name
                    name.clone()
                }
                RustType::TypeParam(rust_type_param) => {
                    format!("for__{}", rust_type_param.name)
                }
                RustType::Option(inner) => {
                    // The idea here is to differeniate between eg Option<T> and Option<i32>
                    let generic_name = match &inner.type_ {
                        RustTypeParamValue::Unresolved => inner.name.clone(),
                        RustTypeParamValue::RustType(resolved) => rust_type_js_name(resolved),
                    };
                    format!("Option_{}_", generic_name)
                }
                RustType::I32 => {
                    // The idea here is to differeniate between eg Option<T> and Option<i32>
                    "i32".to_string()
                }
                _ => {
                    dbg!(rust_type);
                    todo!()
                }
            }
        }
        let target_name = rust_type_js_name(&self.target);
        Ident::NoConversion(format!("{trait_name}__{target_name}"))
    }
}

#[derive(Debug, Clone)]
struct RustImplItemNoJs {
    ident: String,
    item: RustImplItemItemNoJs,
    // return_type: RustType,
    // syn_object: ImplItem,
}

// TODO clean up these types since eg there is duplication of the fn ident
#[derive(Debug, Clone)]
struct RustImplItemJs {
    ident: String,
    item: RustImplItemItemJs,
    // return_type: RustType,
    // syn_object: ImplItem,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
enum RustImplItemItemNoJs {
    /// (static, fn info),
    Fn(bool, FnInfo),
    Const,
}

#[derive(Debug, Clone)]
enum RustImplItemItemJs {
    /// (static, fn info, js fn),
    Fn(bool, FnInfo, JsFn),
    Const(JsLocal),
}

// #[derive(Debug, Clone)]
// struct MyGeneric {
//     name: String,
//     type_: RustType,
// }

// #[derive(Debug, Clone)]
// pub enum VarOrFnInfo {
//     ScopedVar(ScopedVar),
//     FnInfo(FnInfo)
// }

/// variable: Vec<ScopedVar>,
/// fns: Vec<FnInfo>,
/// generics: Vec<MyGeneric>,
/// structs_enums: Vec<StructOrEnumMethods>,
/// TODO remove `Default` to prevent accidentally creating `GlobalDataScope`s with a scope_id of []
#[derive(Debug, Clone)]
struct GlobalDataScope {
    // NOTE techincally we don't need this but it is useful to be able to reconcile with the static/preprocessed scopes to ensure we are talking about the same thing
    scope_id: Vec<usize>,
    variables: Vec<ScopedVar>,
    // fns: Vec<FnInfo>,
    /// Why does a scope have generics?? for fns/methods?
    // generics: Vec<MyGeneric>,
    // generics: Vec<RustTypeParam>,
    // Need to keep track of where the generic is used, eg input to enum variant, input to method, result of some fn call in the body, etc so that when eg we have Foo::Bar(T) getting instantiated with `let foo = Foo::Bar(5)`, we know to then update the type of T to be i32
    // item_definitons: Vec<ItemDefinition>,
    /// TODO I think we want to hoist all scoped impl blocks to the module level, ie get them during the first pass? Well traits can't be hoisted because traits can shadow the names of existing traits in parents scopes, but if we try to impl a shadowed trait you get a "multiple applicable items in scope" (I think this just means you need an explicit pah like <MyType as Trait1> or something ???) error so I think hoisting impls is fine, and potentially necessary because you can use an impl'd method in a parent scope of the impl or even a different scope branch, ie rustc seems to hoist impl blocks, the only limitation is that the any impl'd trait and the target type are (of course) in scope of the impl block. NO - can't hoist `impl MyTrait` blocks because if there is multiple scoped `MyTrait`s (ie duplicate names), we won't know which `impl MyTrait` to use.
    ///
    ///
    /// I think the solution is as follows. First we need to consider normal impls separately to trait impls.
    /// 1. Normal impls can be scoped and duplicated if the type they are implementing is duplicated/shadowed, so can't just hoist all of them to module level, so the impl blocks need to be scoped but we do need to hoist them to the same scope as the type definition, give the methods/items implemented can be used before the impl and in parent scopes
    /// 2. Trait impls are different. For normal impls we cannot apply say `impl Foo { fn foo() {} }` multiple times for the same item/type, whereas for trait impls we can have duplicate, identical impls say `impl Bar for Foo { fn foo() {} }`, as long as we also have multiple `Bar`s, ie the trait is shadowed/duplicated. Also, we cannot call any methods unless the trait for which the method was implemented is in scope, so in this case we want to hoist the impl block to *either* the same scope as the trait or the same scope as the (concrete type implemented)
    /// 3. For trait impls of generic types...
    ///
    ///
    /// Also need to consider scoped use stmts, which I don't know if the first pass handles?
    ///
    /// If we tried to premptively add all impl items for each type, we would have to differentiate between say Foo<i32> and Foo<f32>, and so we would have to add records for every possible concrete type param. For this reason it is better to just store the impl blocks as is, with the trait and generic info eg `impl MyTrait for T` and then work out which impl blocks apply to which type/method as and when needed, ie we are looking up info on a method.
    /// (ident, type)
    // impl_blocks: Vec<(RustType, Vec<RustImplItem>)>,
    // I think it is easier to just store the syn object because we need so much info, and I don't think we will be creating impl blocks manually so don't *need* our own type
    // Also need to consider (possibly multiple) impls being defined in different modules to the target type
    // impl_blocks: Vec<ItemImpl>,
    // impl_blocks: Vec<RustImplBlock>,
    // trait_definitons: Vec<RustTypeImplTrait>,
    // trait_definitons: Vec<RustTraitDefinition>,
    // consts: Vec<ConstDef>,
    /// Blocks, match arms, closures, etc are differnt to fn scopes because they can access variables from their outer scope. However, they are similar in that you loose all the items and variables (not impls though) defined in them, at the end of their scope. This is a flag to indicate this type of scope and thus when looking for things such as variables, we should also look in the surrounding scope.
    _look_in_outer_scope: bool,
    use_mappings: Vec<(String, Vec<String>)>,
}

#[derive(Debug, Clone)]
struct GlobalData {
    _crate_path: Option<PathBuf>,
    modules: Vec<ModuleData>,
    _crates: Vec<CrateData>,
    // TODO doesn't handle capturing scopes which needs rules to mimic how a closure decides to take &, &mut, or ownership
    // NOTE use separate Vecs for vars and fns because not all scopes (for vars) eg blocks are fns
    // NOTE don't want to pop fn after we finish parsing it because it will be called later in the same scope in which it was defined (but also might be called inside itself - recursively), so only want to pop it once it's parent scope completes, so may as well share scoping with vars
    // NOTE need to store vars and fns in the same Vec to ensure we know the precendence in cases like `fn foo() {}; fn bar() {}; let foo = bar;` NO - functions are hoisted so we always want to check if a var with that ident exists first *then* look for a fn, first in the scopes, then at the module level
    scopes: Vec<GlobalDataScope>,
    // TODO combine this with impl_items
    // struct_or_enum_methods: Vec<StructOrEnumMethods>,
    // scopes: Vec<Vec<ScopedVar>>,
    /// (the purpose originally was for self not Self... which is not needed, but Self is neccessary) the purpose of this is for storing the type of `Self` *not* `self`, eg if a impl fn body contains `let foo: Self = Self {};`, we will want to know what Self is so we know the types of `foo.some_field` etc
    ///
    /// We have a Vec in case there is an impl block nested inside an impl block?
    impl_block_target_type: Vec<RustType>,
    /// Similar to impl_block_target_type but if for storing type params of the impl eg `impl<A, B> Foo<A, B> { ... }` so that when `A` and `B` appears in one of the impl's item definitions and we try and lookup the path `A` and `B` with `resolve_path()` we can also look here to find the type params.
    /// TODO Should be Vec of Vecs for same reason impl_block_target_type is a Vec
    _impl_block_type_params: Vec<RustTypeParam>,
    // TODO handle closures - which don't have explicitly specified return type, need to infer it from return value
    // scoped_fns: Vec<ItemFn>,
    rust_prelude_types: RustPreludeTypes,
    // TODO why have this seprate to module.item_def? Because they aren't defined anywhere and are available in all modules so don't really belong to a module?
    // (rust name, js primative type name, item definition)
    // TODO why store rust name when it is in ItemDefinition???
    // TODO should this just be a big struct rather than a vec so we don't have to do lookups?
    // rust_prelude_definitions: Vec<(String, String, ItemDefinition)>,
    /// (trait name, impl item)
    default_trait_impls: Vec<(String, JsImplItem)>,
    /// Used for working out which `default_trait_impls` elements to add to which classes
    ///
    /// (class name, trait name)
    _default_trait_impls_class_mapping: Vec<(String, String)>,
    /// For temporary storage of JS methods prior to adding to JS classes
    /// TODO doesn't seem like we are actually populating this even though it has been used for a while?
    // impl_items_for_js: Vec<ImplItemTemp>,
    /// For looking up return types of methods etc
    // impl_items_for_types: Vec<(RustType, RustImplItem)>,
    // We keep the impl blocks at the crate level rather than in the relevant Module because different it is not possible to impl the same eg method name on the same struct, even using impl blocks in completely separate modules. Impl item idents must be unique for a given type across the entire crate. I believe this is also the case for scoped impls? This is because impl'd items are available on the item definition/instance they are targetting, not only in parent scopes, but also parent modules.
    // impl_blocks: Vec<ItemImpl>,
    impl_blocks: Vec<JsImplBlock2>,
    /// The purpose of having this here is so that all crate scoped impl blocks are immeditately available when parsing the syn to JS, eg if we come across a class (module level or scoped), we want to be able to add the methods which are implemented for it at that point, but these impls might not appear until later in the code, so instead we popualte scoped_impl_blocks in extract_data_populate_item_definitions to ensure it is available
    /// Given method names (impld for the same item) must be unqiue across the crate, for module level impls, we can just store all impl blocks in a big list, and add all their methods to all matching classes/items/enums/structs, whether the method is private/public is irrelevant since if it has been defined it must/should get used at some point.
    /// Scoped impls are a litte more complicated though, because in the same way we distinguish between different module level structs with the same name by taking into account their module path, for scoped structs we need to take into account the scope, ie a scoped `impl Foo { ... }` should only be applied to the first `Foo` that is found in parent scopes, else any module (of course taking into account the full module path used in `impl Foo { ... }`), because there might be another `Foo` in a higher scope with the same method impld, so we must not apply it there.
    /// We don't have to
    /// ((module path, scope id), rust impl block))
    #[allow(clippy::type_complexity)]
    _scoped_impl_blocks: Vec<((Vec<String>, Vec<usize>), JsImplBlock2)>,
    /// Testing: for the purpose of populating `item_definition.impl_items` see if we can store less info about impl blocks. We need the "signature" to be parsed so that we can easily determine whether the target is a type param or concrete type (or mixture - TODO), and also id's for the traits involved, ie the bounds on generics and the trait being impl.
    impl_blocks_simpl: Vec<RustImplBlockSimple>,
    duplicates: Vec<Duplicate>,
    transpiled_modules: Vec<JsModule>,
    // /// For keeping track of whether we are parsing items at the module level or in a fn scope, so that we know whether we need to add the items to `.scopes` or not.
    // at_module_top_level: bool,
    // 1 based
    scope_id: Vec<usize>,
    // 1 based
    scope_count: Vec<usize>,
}
impl GlobalData {
    fn new(crate_path: Option<PathBuf>) -> GlobalData {
        // let option_def = ItemDefinition {
        //     ident: "Option".to_string(),
        //     is_copy: false,
        //     generics: vec!["T".to_string()],
        //     struct_or_enum_info: StructOrEnumDefitionInfo::Enum(EnumDefinitionInfo {
        //         members: vec![
        //             EnumVariantInfo {
        //                 ident: "Some".to_string(),
        //                 inputs: vec![EnumVariantInputsInfo::Unnamed(RustType::TypeParam(
        //                     RustTypeParam {
        //                         name: "T".to_string(),
        //                         type_: RustTypeParamValue::Unresolved,
        //                     },
        //                 ))],
        //             },
        //             EnumVariantInfo {
        //                 ident: "None".to_string(),
        //                 inputs: Vec::new(),
        //             },
        //         ],
        //     }),
        //     impl_block_ids: Vec::new(),
        // };

        // let ravascript_prelude_crate = CrateData {
        //     name: "ravascript".to_string(),
        // };

        // let mut impl_blocks_simpl = Vec::new();
        // let mut rust_items = Vec::new();
        // rust_items.push(RustImplItemNoJs {
        //     ident: "is_some_and".to_string(),
        //     item: RustImplItemItemNoJs::Fn(
        //         false,
        //         false,
        //         FnInfo {
        //             ident: "is_some_and".to_string(),
        //             inputs_types: vec![RustType::Closure(
        //                 vec![RustType::TypeParam()],
        //                 RustType::Bool,
        //             )],
        //             generics: (),
        //             return_type: (),
        //         },
        //     ),
        //     syn_object: (),
        // });
        // impl_blocks_simpl.push(RustImplBlockSimple {
        //     unique_id: "is this needed?".to_string(),
        //     generics: vec![RustGeneric {
        //         ident: "T".to_string(),
        //         trait_bounds: Vec::new(),
        //     }],
        //     trait_: None,
        //     target: RustType::Option(Box::new(RustType::TypeParam(RustTypeParam {
        //         name: "T".to_string(),
        //         type_: RustTypeParamValue::Unresolved,
        //     }))),
        //     rust_items,
        //     // TODO do we need this??
        //     items: Vec::new(),
        // });

        // let code = include_str!("rust_prelude/option.rs");
        // let modules = from_file(code, false);
        // assert_eq!(modules.len(), 1);
        // let option_module = &modules[0];

        // for stmt in &option_module.stmts {
        //     match stmt {
        //         JsStmt::Class(js_class) => {
        //             if js_class.name == "Option" {
        //                 prelude_stmts.push(stmt.clone());
        //             }
        //         }
        //         JsStmt::ClassMethod(_, _, _, _) => todo!(),
        //         JsStmt::ClassStatic(_) => todo!(),
        //         // JsStmt::Local(js_local) => match &js_local.lhs {
        //         //     LocalName::Single(name) => {
        //         //         if name == "Some" || name == "None" {
        //         //             js_stmts.insert(0, stmt.clone());
        //         //         }
        //         //     }
        //         //     LocalName::DestructureObject(_) => todo!(),
        //         //     LocalName::DestructureArray(_) => todo!(),
        //         // },
        //         JsStmt::Local(_) => todo!(),
        //         JsStmt::Expr(_, _) => todo!(),
        //         JsStmt::Import(_, _, _) => todo!(),
        //         JsStmt::Function(_) => todo!(),
        //         JsStmt::ScopeBlock(_) => todo!(),
        //         // JsStmt::TryBlock(_) => todo!(),
        //         // JsStmt::CatchBlock(_, _) => todo!(),
        //         JsStmt::Raw(_) => todo!(),
        //         JsStmt::Comment(_) => todo!(),
        //     }
        // }

        GlobalData {
            _crate_path: crate_path,
            modules: Vec::new(),
            // crates: vec![ravascript_prelude_crate],
            _crates: vec![],
            // init with an empty scope to ensure `scopes.last()` always returns something TODO improve this
            scopes: vec![GlobalDataScope {
                scope_id: Vec::new(),
                variables: Vec::new(),
                _look_in_outer_scope: false,
                use_mappings: Vec::new(),
            }],
            // struct_or_enum_methods: Vec::new(),
            impl_block_target_type: Vec::new(),
            _impl_block_type_params: Vec::new(),
            // scoped_fns: vec![],
            rust_prelude_types: RustPreludeTypes::default(),
            _default_trait_impls_class_mapping: Vec::new(),
            default_trait_impls: Vec::new(),
            // impl_items_for_js: Vec::new(),
            duplicates: Vec::new(),
            transpiled_modules: Vec::new(),
            impl_blocks: Vec::new(),
            _scoped_impl_blocks: Vec::new(),
            impl_blocks_simpl: Vec::new(),
            scope_id: Vec::new(),
            scope_count: vec![0],
            // at_module_top_level: false,
        }
    }

    fn scope_id_as_option(&self) -> Option<Vec<usize>> {
        if self.scope_id.is_empty() {
            None
        } else {
            Some(self.scope_id.clone())
        }
    }

    fn push_new_scope(&mut self, look_in_outer_scope: bool, variables: Vec<ScopedVar>) {
        let scope_count = {
            let scope_count = self.scope_count.last_mut().unwrap();
            *scope_count += 1;
            *scope_count
        };
        self.scope_id.push(scope_count);
        let var_scope = GlobalDataScope {
            scope_id: self.scope_id.clone(),
            variables,
            _look_in_outer_scope: look_in_outer_scope,
            use_mappings: Vec::new(),
        };
        self.scopes.push(var_scope);
        self.scope_count.push(0);
    }
    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.scope_count.pop();
        self.scope_id.pop();
    }

    // handling paths:
    // A path could be:
    // a var (path.len()=1)
    // an item (ie a struct, enum, or fn)
    // a module (only if path.len()>1 and the final element cannot be a module)
    // We can't just look up the item name in global_data, because multiple modules might have the same item name. So the first thing we want to do is determine the module path. eg resolve self/super/crate, follow any use stmts etc given the path might a single element specificying an item name but that item is actually used from a differnt module. If there is no module path, ie the first element is not self/super/crate/some module, and the item has not been use'd into scope, then look up through the scopes to find it (could be a var (if path.len=1 and only in certain postitions like assignments and args, not eg the type of an input) or a scoped item). I believe we don't have to worry about scoped modules because we look for them during the first pass so they should be included in global_data???
    // Can actually just look up scoped vars/items first??
    //
    // remember we want two thing when lookup up paths:
    // 1. In all cases (I think?), to find the type of whatever it points to
    // 2. In cases where we need to transpile the path, to determine what the transpiled path should be, eg what is the namespaced JS ident for the item.

    /// Needs to be extended to also lookup module level items (and also return namespaced JS path?)
    // fn lookup_scoped_var_or_item_definiton(&self, path: &Vec<String>) -> Option<ItemDefinition> {
    // Need to know eg if a var is mut, so can't just return RustType(??) could mut be added as a RustType??
    // fn lookup_scoped_var_or_item_definiton(&self, path: &Vec<String>) -> Option<RustType> {
    // fn lookup_scoped_var_or_item_definiton(&self, path: &Vec<String>) -> Option<VarItemFn> {
    //     self.scopes.iter().rev().find_map(|scope| {
    //         // Note variables, and items definitions can't shadow each other in the same scope, so don't need to worry about the order in which each var and item definition was defined in the scope, ie vars and item definitions don't need to be together sorted in one big list, can't just look through the list of vars *then* look through the list of item definitions.
    //         let first = &path[0];
    //         let var = scope.variables.iter().find(|v| &v.name == first);
    //         let func = scope.fns.iter().find(|f| &f.ident == first);
    //         let se = scope.item_definitons.iter().find(|se| &se.ident == first);
    //         if path.len() == 1 {
    //             if let Some(var) = var {
    //                 return Some(VarItemFn::Var(var.clone()));
    //             } else if let Some(func) = func {
    //                 return Some(VarItemFn::Fn(func.clone()));
    //             } else if let Some(se) = se {
    //                 return Some(VarItemFn::StructOrEnum(se.clone()));
    //             } else {
    //                 return None;
    //             }
    //         } else if path.len() == 2 {
    //             // if let Some(var) = var {
    //             //     return Some(VarItemFn::Var(var.clone()));
    //             // } else if let Some(func) = func {
    //             //     return Some(VarItemFn::Fn(func.clone()));
    //             // } else if let Some(se) = se {
    //             //     return Some(VarItemFn::StructOrEnum(se.clone()));
    //             // } else {
    //             //     return None;
    //             // }
    //             todo!()
    //         } else {
    //             todo!()
    //         }
    //     })
    // }

    // fn lookup_fn_definition_known_module(
    //     &self,
    //     name: String,
    //     module_path: &Option<Vec<String>>,
    // ) -> FnInfo {
    //     if let Some(module_path) = module_path {
    //         let module = self
    //             .modules
    //             .iter()
    //             .find(|m| &m.path == module_path)
    //             .unwrap();
    //         module
    //             .fn_info
    //             .iter()
    //             .cloned()
    //             .find(|se| se.ident == name)
    //             .unwrap()
    //     } else {
    //         self.scopes
    //             .iter()
    //             .rev()
    //             .find_map(|s| s.fns.iter().rev().cloned().find(|se| se.ident == name))
    //             .unwrap()
    //     }
    // }

    // fn lookup_item_definition_known_module(
    //     &self,
    //     name: &String,
    //     module_path: &Option<Vec<String>>,
    // ) -> ItemDefinition {
    //     if let Some(module_path) = module_path {
    //         let module = self
    //             .modules
    //             .iter()
    //             .find(|m| &m.path == module_path)
    //             .unwrap();
    //         module
    //             .item_definitons
    //             .iter()
    //             .cloned()
    //             .find(|se| &se.ident == name)
    //             .unwrap()
    //     } else {
    //         self.scopes
    //             .iter()
    //             .rev()
    //             .find_map(|s| {
    //                 s.item_definitons
    //                     .iter()
    //                     .rev()
    //                     .cloned()
    //                     .find(|se| &se.ident == name)
    //             })
    //             .unwrap()
    //     }
    // }

    fn syn_type_to_rust_type_struct_or_enum(
        &self,
        current_module: &[String],
        // generics: &Vec<RustTypeParam>,
        syn_type: &Type,
    ) -> (Vec<RustTypeParam>, Vec<String>, Option<Vec<usize>>, String) {
        let type_path = match syn_type {
            Type::Path(type_path) => {
                type_path
                    .path
                    .segments
                    .iter()
                    .map(|seg| {
                        RustPathSegment {
                            ident: seg.ident.to_string(),
                            turbofish: match seg.arguments {
                                PathArguments::None => Vec::new(),
                                // TODO support nested turbofish types
                                PathArguments::AngleBracketed(_) => {
                                    // TODO this is a hack, needs handling properly
                                    Vec::new()
                                }
                                PathArguments::Parenthesized(_) => todo!(),
                            },
                        }
                    })
                    .collect::<Vec<_>>()
            }
            _ => todo!(),
        };

        let (module_path, item_path, item_scope_id) = resolve_path(
            false,
            false,
            true,
            type_path,
            self,
            current_module,
            current_module,
            &self.scope_id_as_option(),
        );
        assert!(item_path.len() == 1);
        // dbg!("yes");
        let item_def = self.lookup_item_def_known_module_assert_not_func2(
            &module_path,
            &item_scope_id,
            &item_path[0].ident,
        );
        // dbg!("ytes222");
        (
            item_def
                .generics
                .iter()
                .map(|gen| RustTypeParam {
                    name: gen.clone(),
                    type_: RustTypeParamValue::Unresolved,
                })
                .collect::<Vec<_>>(),
            module_path,
            item_scope_id,
            item_path[0].ident.clone(),
        )
    }

    // fn lookup_item_def_known_module_assert_not_func(
    //     &self,
    //     module_path: &Option<Vec<String>>,
    //     name: &String,
    // ) -> ItemDefinition {
    //     if let Some(module_path) = module_path {
    //         let module = self
    //             .modules
    //             .iter()
    //             .find(|m| &m.path == module_path)
    //             .unwrap();

    //         let func = module.fn_info.iter().find(|se| &se.ident == name);
    //         assert!(func.is_none());

    //         module
    //             .item_definitons
    //             .iter()
    //             .find(|se| &se.ident == name)
    //             .unwrap()
    //             .clone()
    //     } else {
    //         // Look for scoped items
    //         // dbg!(&self.scopes);
    //         self.scopes
    //             .iter()
    //             .rev()
    //             .find_map(|scope| {
    //                 let var = scope.variables.iter().find(|v| &v.name == name);
    //                 let func = scope.fns.iter().find(|f| &f.ident == name);
    //                 assert!(var.is_none() && func.is_none());

    //                 scope.item_definitons.iter().find(|f| &f.ident == name)
    //             })
    //             .unwrap()
    //             .clone()
    //     }
    // }

    fn lookup_fn_info_known_module(
        &self,
        module_path: &[String],
        scope_id: &Option<Vec<usize>>,
        name: &str,
    ) -> FnInfo {
        let module = self.modules.iter().find(|m| m.path == module_path).unwrap();
        let scoped_fn_info = scope_id
            .as_ref()
            .and_then(|scope_id| {
                module
                    .scoped_various_definitions
                    .iter()
                    .find(|svd| &svd.0 == scope_id)
            })
            .and_then(|svd| svd.1.fn_info.iter().find(|fn_info| fn_info.ident == name));
        let module_fn_info = module
            .various_definitions
            .fn_info
            .iter()
            .find(|fn_info| fn_info.ident == name);

        scoped_fn_info.or(module_fn_info).unwrap().clone()
    }

    fn lookup_item_def_known_module_assert_not_func2(
        &self,
        module_path: &[String],
        scope_id: &Option<Vec<usize>>,
        name: &str,
    ) -> ItemDefinition {
        let module = self.modules.iter().find(|m| m.path == module_path).unwrap();
        let scoped_item_def = scope_id
            .as_ref()
            .and_then(|scope_id| {
                module
                    .scoped_various_definitions
                    .iter()
                    .find(|svd| &svd.0 == scope_id)
            })
            .and_then(|svd| {
                svd.1
                    .item_definitons
                    .iter()
                    .find(|item_def| item_def.ident == name)
            });
        let module_item_def = module
            .various_definitions
            .item_definitons
            .iter()
            .find(|item_def| item_def.ident == name);

        // Dont't need to look for prelude items here since resolve_path already returns a ["prelude_special_case"] module for prelude types. This seems like a better place though, since then we wouldn't need a special module name - well resolve_path still needs to return something? maybe [""] instead?
        // let prelude_item_def = self
        //     .rust_prelude_definitions
        //     .iter()
        //     .find_map(|(_name, _js_name, item_def)| (&item_def.ident == name).then_some(item_def));

        // Might want to check/assert these or useful for debugging
        let _is_box_prelude =
            module_path == [PRELUDE_MODULE_PATH] && scope_id.is_none() && name == "Box";

        // if let Some(item_def) = scoped_item_def.or(module_item_def).or(prelude_item_def) {
        if let Some(item_def) = scoped_item_def.or(module_item_def) {
            item_def.clone()
        } else {
            dbg!("could find item def for lookup_item_def_known_module_assert_not_func2");
            dbg!(&module_path);
            dbg!(&scope_id);
            dbg!(&name);
            panic!();
        }
    }

    // NOTE don't need this because we are already resolving paths with get_path()??
    // This looks up *any ident/Path in a module*, so the item might not actually be defined in the current module and has been use'd. We also need to return the module path of the item, which of course might not be the current module, because this item definition will go into a rust type which will need to look up the item definition
    // Alternatively could just make sure we store all items that are either defined *or* use'd in a module. This should work because module level item names must still be unique even if they are just use'd rather than defined, and currently we aren't yet acutally populating modules.item_definition with anything anyway. TODO come back to this once codebase has been cleaned up and simplified and just handle single module cases for now.
    /// returns None if no item is found
    ///
    /// Option<moule path> will be None for scoped items
    ///
    ///  -> Option<(Option<moule path>, item definition)>
    ///
    /// Used for looking up types eg `fn my_fun(my_arg: some::path::to::a::Type) {}`, not vars
    // fn lookup_item_definition_any_module(
    //     &self,
    //     current_module_path: &Vec<String>,
    //     path: &Vec<String>,
    //     // current_module: &Vec<String>,
    // ) -> Option<(Option<Vec<String>>, ItemDefinition)> {
    //     let first = &path[0];
    //     // TODO should just use get_path to look for scoped items?
    //     if let Some(item_def) = self.lookup_scoped_item_definiton(first) {
    //         return Some((None, item_def));
    //     } else {
    //         // dbg!(path);
    //         // dbg!(current_module_path);
    //         let (item_module_path, item_path, _is_scoped) = get_path(
    //             false,
    //             false,
    //             true,
    //             path.iter()
    //                 .map(|seg| RustPathSegment {
    //                     ident: seg.clone(),
    //                     turbofish: Vec::new(),
    //                 })
    //                 .collect::<Vec<_>>(),
    //             self,
    //             current_module_path,
    //             current_module_path,
    //             &None,
    //         );
    //         // dbg!(&item_module_path);
    //         // dbg!(&item_path);

    //         if item_module_path == vec!["prelude_special_case".to_string()] {
    //             // Get prelude item definitions
    //             let (_name, def) = self
    //                 .rust_prelude_definitions
    //                 .iter()
    //                 .find(|(name, def)| name == &item_path[0].ident)
    //                 .unwrap();
    //             return Some((Some(item_module_path), def.clone()));
    //         }

    //         let item_module = self
    //             .modules
    //             .iter()
    //             .find(|m| &m.path == &item_module_path)
    //             .unwrap();
    //         // dbg!(&item_module);
    //         // dbg!(&item_path[0].ident);

    //         // TODO if the path is eg an associated fn, should we return the item or the fn? ie se or RustType?
    //         let item_def = item_module
    //             .item_definitons
    //             .iter()
    //             .find(|se| se.ident == item_path[0].ident);

    //         if let Some(item_def) = item_def {
    //             return Some((Some(item_module_path), item_def.clone()));
    //         } else {
    //             todo!()
    //         }

    //         // let item_definition = self.
    //         // let module_item_definition = current_module.item_definitons.iter().find(|se| &se.ident == path);
    //         // if let Some(item_def) = scoped_item_definition {
    //         //     // todo!();
    //         //     Some((None, item_def.clone()))
    //         // } else if let Some(item_def) = module_item_definition {
    //         //     // todo!();
    //         //     Some((Some(current_module), item_def.clone()))
    //         // } else {
    //         //     None
    //         // }
    //     }
    // }

    /// NOTE to be used pre syn -> JS parsing, ie self.scopes won't have been populated
    // -> (module path, scope id (even if we are in a scope so `Some` scope id is provided, the item being looked up might still be module level. Of course if None scope id is provided it is impossible for a Some scope id to be returned), item definition)
    fn lookup_item_definition_any_module_or_scope(
        &self,
        current_module_path: &[String],
        scope_id: &Option<Vec<usize>>,
        path: &[String],
    ) -> (Vec<String>, Option<Vec<usize>>, ItemDefinition) {
        // dbg!(&path);
        // dbg!(&current_module_path);
        // dbg!(&scope_id);
        // dbg!("get_path");
        let (item_module_path, item_path, item_scope) = resolve_path(
            false,
            true,
            true,
            path.iter()
                .map(|seg| RustPathSegment {
                    ident: seg.clone(),
                    turbofish: Vec::new(),
                })
                .collect::<Vec<_>>(),
            self,
            current_module_path,
            current_module_path,
            scope_id,
        );
        // dbg!(&item_module_path);
        // dbg!(&item_path);
        // dbg!(&item_scope);

        let item_module = self
            .modules
            .iter()
            .find(|m| m.path == item_module_path)
            .unwrap();

        // TODO if the path is eg an associated fn, should we return the item or the fn? ie se or RustType?
        let item_defintions = if let Some(scope_id) = &item_scope {
            &item_module
                .scoped_various_definitions
                .iter()
                .find(|svd| &svd.0 == scope_id)
                .unwrap()
                .1
                .item_definitons
        } else {
            &item_module.various_definitions.item_definitons
        };
        let item_def = item_defintions
            .iter()
            .find(|se| se.ident == item_path[0].ident)
            .unwrap();

        (item_module_path, item_scope, item_def.clone())
    }

    // TODO should also look up fns?
    // fn lookup_scoped_item_definiton(&self, name: &String) -> Option<ItemDefinition> {
    //     self.scopes
    //         .iter()
    //         .rev()
    //         .find_map(|s| s.item_definitons.iter().rev().find(|se| &se.ident == name))
    //         .cloned()
    // }

    fn lookup_trait_definition_any_module<I>(
        &self,
        current_module_path: &[String],
        current_scope_id: &Option<Vec<usize>>,
        // path: &Vec<String>,
        path: I,
        // current_module: &Vec<String>,
    ) -> (Vec<String>, Option<Vec<usize>>, RustTraitDefinition)
    where
        // I: IntoIterator<Item = String>,
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        // dbg!("lookup_trait_definition_any_module");
        let (item_module_path, item_path, item_scope) = resolve_path(
            false,
            true,
            true,
            path.into_iter()
                .map(|seg| RustPathSegment {
                    ident: seg.as_ref().to_string(),
                    turbofish: Vec::new(),
                })
                .collect::<Vec<_>>(),
            self,
            current_module_path,
            current_module_path,
            current_scope_id,
        );
        let item_module = self
            .modules
            .iter()
            .find(|m| m.path == item_module_path)
            .unwrap();

        let trait_definiton = if let Some(item_scope) = &item_scope {
            let svd = item_module
                .scoped_various_definitions
                .iter()
                .find(|svd| &svd.0 == item_scope)
                .unwrap();
            svd.1
                .trait_definitons
                .iter()
                .find(|trait_def| trait_def.name == item_path[0].ident)
        } else {
            item_module
                .various_definitions
                .trait_definitons
                .iter()
                .find(|t| t.name == item_path[0].ident)
        };
        (
            item_module_path,
            item_scope,
            trait_definiton.unwrap().clone(),
        )
    }

    #[allow(dead_code)]
    fn get_module_mut(&mut self, module_path: &[String]) -> &mut ModuleData {
        self.modules
            .iter_mut()
            .find(|m| m.path == module_path)
            .unwrap()
    }

    // This Doesn't/shouldn't look up methods as far as I can tell (methods are always handled directly in handle_expr_method_call) so rename
    // fn lookup_method_or_associated_fn(
    fn lookup_associated_fn(
        &self,
        item_generics: &[RustTypeParam],
        item_module_path: &[String],
        item_scope_id: &Option<Vec<usize>>,
        sub_path: &RustPathSegment,
        _item_path_seg: &str,
        item_def: &ItemDefinition,
        // ) -> Option<PartialRustType> {
    ) -> Option<RustType> {
        // let impl_method = self.lookup_impl_item_item(
        //     item_generics,
        //     item_module_path,
        //     item_scope_id,
        //     sub_path,
        //     item_path_seg,
        //     item_def,
        // );

        let impl_method = self.lookup_impl_item_item2(item_def, sub_path);

        // let impl_method = if let Some((used, impl_method)) = impl_method {
        //     match impl_method.item {
        //         RustImplItemItem::Fn(private, static_, fn_info, js_fn) => {
        //             // If turbofish exists on fn path segment then use that for type params, otherwise use the unresolved params defined on the fn definition
        //             let fn_generics = if sub_path.turbofish.len() > 0 {
        //                 sub_path
        //                     .turbofish
        //                     .iter()
        //                     .enumerate()
        //                     .map(|(i, g)| RustTypeParam {
        //                         name: fn_info.generics[i].clone(),
        //                         type_: RustTypeParamValue::RustType(Box::new(g.clone())),
        //                     })
        //                     .collect::<Vec<_>>()
        //             } else {
        //                 // NOTE for now we are assuming turbofish must exist for generic items, until we implement a solution for getting type params that are resolved later in the code
        //                 assert!(fn_info.generics.len() == 0);
        //                 fn_info
        //                     .generics
        //                     .iter()
        //                     .map(|g| RustTypeParam {
        //                         name: g.clone(),
        //                         type_: RustTypeParamValue::Unresolved,
        //                     })
        //                     .collect::<Vec<_>>()
        //             };

        //             // Some(PartialRustType::RustType(RustType::Fn(
        //             //     Some(item_generics.clone()),
        //             //     fn_generics,
        //             //     item_module_path.clone(),
        //             //     RustTypeFnType::AssociatedFn(item_def.ident, sub_path.ident),
        //             // )))
        //             Some(RustType::Fn(
        //                 Some(item_generics.clone()),
        //                 fn_generics,
        //                 item_module_path.clone(),
        //                 item_scope_id.clone(),
        //                 RustTypeFnType::AssociatedFn(
        //                     item_def.ident.clone(),
        //                     sub_path.ident.clone(),
        //                 ),
        //             ))
        //         }
        //         RustImplItemItem::Const(_) => todo!(),
        //     }
        // } else {
        //     None
        // };
        // impl_method

        let impl_method = if let Some(impl_method) = impl_method {
            match impl_method.item {
                RustImplItemItemNoJs::Fn(_static_, fn_info) => {
                    // If turbofish exists on fn path segment then use that for type params, otherwise use the unresolved params defined on the fn definition
                    let fn_generics = if !sub_path.turbofish.is_empty() {
                        sub_path
                            .turbofish
                            .iter()
                            .enumerate()
                            .map(|(i, g)| RustTypeParam {
                                name: fn_info.generics[i].clone(),
                                type_: RustTypeParamValue::RustType(Box::new(g.clone())),
                            })
                            .collect::<Vec<_>>()
                    } else {
                        // NOTE for now we are assuming turbofish must exist for generic items, until we implement a solution for getting type params that are resolved later in the code
                        assert!(fn_info.generics.is_empty());
                        fn_info
                            .generics
                            .iter()
                            .map(|g| RustTypeParam {
                                name: g.clone(),
                                type_: RustTypeParamValue::Unresolved,
                            })
                            .collect::<Vec<_>>()
                    };

                    // Some(PartialRustType::RustType(RustType::Fn(
                    //     Some(item_generics.clone()),
                    //     fn_generics,
                    //     item_module_path.clone(),
                    //     RustTypeFnType::AssociatedFn(item_def.ident, sub_path.ident),
                    // )))
                    Some(RustType::Fn(
                        Some(item_generics.to_vec()),
                        fn_generics,
                        item_module_path.to_vec(),
                        item_scope_id.clone(),
                        RustTypeFnType::AssociatedFn(
                            item_def.ident.clone(),
                            sub_path.ident.clone(),
                        ),
                    ))
                }
                RustImplItemItemNoJs::Const => todo!(),
            }
        } else {
            None
        };
        impl_method
    }

    fn lookup_impl_item_item2(
        &self,
        // item_generics: &Vec<RustTypeParam>,
        // item_module_path: &Vec<String>,
        // item_scope_id: &Option<Vec<usize>>,
        item_def: &ItemDefinition,
        sub_path: &RustPathSegment,
    ) -> Option<RustImplItemNoJs> {
        // let module = self
        //     .modules
        //     .iter()
        //     // .find(|m| &m.path == current_module)
        //     .find(|m| &m.path == item_module_path)
        //     .unwrap();

        let impl_method = item_def.impl_block_ids.iter().find_map(|impl_block_id| {
            // TODO also look for scoped impl blocks
            // TODO take into account item type params for generic impls

            // let module_impl_blocks = self.impl_blocks_simpl.iter();
            // let scoped_impl_blocks = module
            //     .scoped_various_definitions
            //     .iter()
            //     .map(|svd| &svd.2)
            //     .flatten();
            // module_impl_blocks
            //     .chain(scoped_impl_blocks)

            // TODO should we be looking through multiple blocks here or should be deuplicate `impl_blocks_simpl` after it is created??
            // I think we could dedupe (as long as we take into account scope) be it is impossible to have duplicate method names, however for impls like `impl<T> Foo for T` maybe we want to keep the impl blocks separate like they are in the original code??? Yes but we can still just dedupe the `RustImplBlockSimple` and keep the `JsImplBlock2` separate. It seems better to just look through multiple blocks here as that is easier than deduplicating and merging items.
            self.impl_blocks_simpl
                .iter()
                .filter(|ibs| &ibs.unique_id == impl_block_id)
                .find_map(|rust_impl_block_simple| {
                    rust_impl_block_simple
                        .rust_items
                        .iter()
                        .find(|rust_item| rust_item.ident == sub_path.ident)
                        .cloned()
                })
        });
        // dbg!(&impl_method);
        // dbg!(&item_def.ident);
        // dbg!(&sub_path.ident);
        // let impl_method = if let Some(impl_method) = impl_method {
        //     impl_method
        // } else {
        //     // dbg!(&global_data.scopes);
        //     dbg!(&item_generics);
        //     dbg!(&item_module_path);
        //     dbg!(&item_scope_id);
        //     dbg!(&sub_path);
        //     // dbg!(&item_name);
        //     dbg!(&item_def);
        //     panic!()
        // };
        impl_method
    }

    // TODO method should just take a RustType rather than specifically an item/struct
    // Looks up the `RustImplItem` eg method for a given item
    // TODO IMPORTANT not handling scoped impls for now for simplicity
    // fn lookup_impl_item_item(
    //     &self,
    //     item_generics: &Vec<RustTypeParam>,
    //     item_module_path: &Vec<String>,
    //     item_scope_id: &Option<Vec<usize>>,
    //     sub_path: &RustPathSegment,
    //     item_name: &String,
    //     item_def: &ItemDefinition,
    //     // ) -> Option<PartialRustType> {
    //     // ) -> Option<(bool, RustImplItem)> {
    // ) -> Option<RustImplItemNoJs> {
    //     // For now focus on supporting explicit gnerics ie turbofish etc so don't have to worry about unresolved types, and module level items so I don't have too much about complex scope shadowing behaviours.

    //     // Look for associated fn of item (struct or enum)

    //     // Look through all impl blocks which match item to find impl item which matches subpath name

    //     // First look for method in direct (non-trait) impls
    //     // TODO for generic structs we need to know the concrete types to know if we should match eg Foo<i32>
    //     // let scoped_impls = self.scopes.iter().map(|s| s.impl_blocks.iter()).flatten();

    //     let module = self
    //         .modules
    //         .iter()
    //         .find(|m| &m.path == item_module_path)
    //         .unwrap();
    //     // let scoped_impls = module.scoped_various_definitions.iter().find_map(|svd| {
    //     //     if let Some(scope_id) = &item_scope_id {
    //     //         if &svd.0 == scope_id {
    //     //             Some(svd.2)
    //     //         } else {
    //     //             None
    //     //         }
    //     //     } else {
    //     //         None
    //     //     }
    //     // });
    //     // let scoped_impls = if let Some(scoped_impls) = scoped_impls {
    //     //     scoped_impls
    //     // } else {
    //     //     Vec::new()
    //     // };
    //     // let scoped_non_trait_impl_method = self
    //     //     .impl_blocks
    //     //     .iter()
    //     //     // .chain(scoped_impls.iter())
    //     //     .find_map(|impl_block| {
    //     //         let types_match = struct_or_enum_types_match(
    //     //             &impl_block.target,
    //     //             &item_generics,
    //     //             &item_module_path,
    //     //             &item_scope_id,
    //     //             &item_def.ident,
    //     //         );

    //     //         // If we have a matching, non-trait impl block, look to see if it contains the method
    //     //         if types_match && impl_block.trait_.is_none() {
    //     //             impl_block
    //     //                 .items
    //     //                 .iter()
    //     //                 .find(|impl_item| impl_item.ident == sub_path.ident)
    //     //                 .cloned()
    //     //         } else {
    //     //             None
    //     //         }
    //     //     });

    //     // Now look for the method in trait impls
    //     // NOTE that while we are looking up a method for a particular `item_def`, we are matching on both concrete impls like `impl<T> Foo for Bar`, and generic impls like `impl<T> Foo for T` or `impl<T: Bar> Foo for T` in which case we need to check if our concrete `item_def` meets the trait bounds of `T`.
    //     // Say we have an impl block like `impl<T> Foo for T`, then we know our item/struct impls Foo and we can add the `impl<T> Foo for T` impl block to the list of impl blocks in which to look for the method.
    //     // However, for an impl block like `impl<T: Bar> Foo for T`, then we first need to determine whether our item/struct implements Bar.
    //     // So the first thing we do is find *all* the traits our item/struct implements
    //     // Note that this is a recursive process, ie once we have found that impl Foo via `impl<T: Bar> Foo for T`, we then assuming that `impl<T: Foo> Baz for T` appears before `impl<T: Bar> Foo for T` in the list of impl blocks, we would need to do another pass of the impl blocks to match it, now that we know we impl Foo. And we should repeatedly do another pass every time we match a new trait impl, until no new trait impls were matched.

    //     // Also note it isn't possible to calculate this out upfront eg in the first pass in `process_items`, because for generic structs it can depend on the concrete types of the generics so will need calculating individually on demand at the point we know the concrete type. For non-generic structs however I think we could calculate up front which will be worth doing at some point for better performance.

    //     // Only need to look in the same or child scopes of the item definition since an impl on an item cannot be in a parent scope
    //     // let possible_scopes = self.scopes.iter().rev().take_while(|s| {
    //     //     s.item_definitons
    //     //         .iter()
    //     //         .any(|item_def| &item_def.ident == item_name)
    //     // });
    //     let possible_scopes = if let Some(item_scope_id) = item_scope_id {
    //         let mut temp_scope_id = item_scope_id.clone();
    //         let mut scopes = Vec::new();
    //         while !temp_scope_id.is_empty() {
    //             let scope = module
    //                 .scoped_various_definitions
    //                 .iter()
    //                 .find(|svd| svd.0 == temp_scope_id)
    //                 .unwrap();
    //             scopes.push(scope.2.clone());
    //             temp_scope_id.pop();
    //         }
    //         scopes.into_iter().flatten().collect::<Vec<_>>()
    //     } else {
    //         Vec::new()
    //     };
    //     let mut all_impl_blocks = possible_scopes;
    //     all_impl_blocks.extend(self.impl_blocks_simpl.clone().into_iter());
    //     // let all_impl_blocks = self.impl_blocks.clone();
    //     dbg!(&all_impl_blocks);

    //     let found_traits = get_traits_implemented_for_item(
    //         &all_impl_blocks,
    //         item_module_path,
    //         item_scope_id,
    //         item_name,
    //     );

    //     // Now we know all the traits the item/struct impls, we can go through all the impl blocks and find the one (if any)
    //     let trait_impl_method = all_impl_blocks
    //         .iter()
    //         .find_map(|impl_block| {
    //             // TODO this needs extending to handle matching any target type, rather than just user structs
    //             match &impl_block.target {
    //                 RustType::TypeParam(rust_type_param) => {
    //                     // TODO should we be looking for/matching on found_traits in the `Foo` or `Bar` of `impl<T: Foo> Bar for T`??? I don't think it actually matter since we seem to be duplicating some work we already did in creating found_traits. NOTE but we must ensure we are matching the correct block, eg Bar might be in found_traits, but doesn't mean we should match because Foo might not be and we might actually get Bar from `impl<T: Baz> Bar for T`, so it is the trait bounds that we must match on.

    //                     // Get bounds on type param
    //                     let type_param_bounds = &impl_block
    //                         .generics
    //                         .iter()
    //                         .find(|generic| generic.ident == rust_type_param.name)
    //                         .unwrap()
    //                         .trait_bounds;

    //                     // If there are no trait bounds ie `impl<T> Foo for T` we always match
    //                     // Does our struct impl all of these traits?
    //                     let struct_impls_all_bounds = type_param_bounds
    //                         .iter()
    //                         .all(|type_param_bound| found_traits.contains(type_param_bound));

    //                     // If so then look for method in impl block
    //                     if struct_impls_all_bounds {
    //                         impl_block
    //                             // .items
    //                             .rust_items
    //                             .iter()
    //                             // .find(|(used, impl_item)| impl_item.ident == sub_path.ident)
    //                             .find(|impl_item| impl_item.ident == sub_path.ident)
    //                     } else {
    //                         None
    //                     }
    //                 }
    //                 RustType::StructOrEnum(
    //                     struct_type_params,
    //                     struct_module_path,
    //                     struct_scope_id,
    //                     struct_name,
    //                 ) => {
    //                     if let Some(impl_trait) = &impl_block.trait_ {
    //                         let types_match = struct_or_enum_types_match(
    //                             &impl_block.target,
    //                             &item_generics,
    //                             &item_module_path,
    //                             item_scope_id,
    //                             &item_def.ident,
    //                         );

    //                         // If types match then look for method in impl block
    //                         if types_match {
    //                             impl_block
    //                                 // .items
    //                                 .rust_items
    //                                 .iter()
    //                                 // .find(|(used, impl_item)| impl_item.ident == sub_path.ident)
    //                                 .find(|impl_item| impl_item.ident == sub_path.ident)
    //                         } else {
    //                             None
    //                         }
    //                         // TODO Trying to get the concrete params at this point doesn't make senese because quite often it is the argument(s) to the associated fn which will determine the concrete params
    //                     } else {
    //                         None
    //                     }
    //                 }
    //                 RustType::MutRef(_) => todo!(),
    //                 RustType::Ref(_) => todo!(),
    //                 _ => todo!(),
    //             }
    //         })
    //         .cloned();

    //     dbg!(&trait_impl_method);

    //     // Now we have all the impl blocks, we can look for the method in said impl blocks
    //     // We also need to check the traits themselves incase the method is a default implementation
    //     // let scoped_traits = self
    //     //     .scopes
    //     //     .iter()
    //     //     .rev()
    //     //     .map(|s| {
    //     //         s.trait_definitons
    //     //             .iter()
    //     //             .filter(|trait_def| found_traits.contains(&(None, trait_def.name.clone())))
    //     //     })
    //     //     .flatten();
    //     let module_level_traits = self
    //         .modules
    //         .iter()
    //         .map(|module| {
    //             module.trait_definitons.iter().filter(|trait_def| {
    //                 found_traits.contains(&(module.path.clone(), None, trait_def.name.clone()))
    //             })
    //         })
    //         .flatten();
    //     // TODO add default impl items to traits
    //     // let default_trait_method = scoped_traits
    //     //     .chain(module_level_traits)
    //     //     .find_map(|trait_def| {
    //     //         trait_def
    //     //             .items
    //     //             .iter()
    //     //             .find(|impl_item| impl_item.ident == sub_path.ident)
    //     //             .cloned()
    //     //     });

    //     // It is not possible to have impls with the same method name, so we should at most match 1 impl item/method
    //     // dbg!(&matched_trait_impl_blocks);
    //     // assert!(
    //     //     matched_trait_impl_blocks
    //     //         .iter()
    //     //         .filter_map(|impl_block| {
    //     //             impl_block
    //     //                 .items
    //     //                 .iter()
    //     //                 .find(|impl_item| impl_item.ident == sub_path.ident)
    //     //         })
    //     //         .count()
    //     //         <= 1
    //     // );
    //     // let module_level_impl_method = matched_trait_impl_blocks
    //     //     .iter()
    //     //     .find_map(|impl_block| {
    //     //         impl_block
    //     //             .items
    //     //             .iter()
    //     //             .find(|impl_item| impl_item.ident == sub_path.ident)
    //     //     })
    //     //     .cloned();

    //     // Use xor because we should not have both a scoped and module level impl method, only either or
    //     // let impl_method = scoped_non_trait_impl_method.xor(trait_impl_method);
    //     let impl_method = trait_impl_method;
    //     impl_method
    // }
}

#[allow(dead_code)]
enum VarItemFn {
    Var(ScopedVar),
    StructOrEnum(ItemDefinition),
    Fn(FnInfo),
}

// TODO I think we want to add *usertype* impls and *impl trait for usertype* (in both cases where the usertype is not generic) as methods to the type's class, but for all other cases, eg *impl trait for T* impls we want to keep the trait impl as an object of fns/class with methods and then still add equivalent methods to the type classes but rather than include the fn body, just point to these fns/methods in the impl block object. This avoids eg potentially duplicating fn bodys on *all* type classes if eg we have `impl<T> MyTrait for T` {}. Though the cost of duplicating the fn bodys should be irrelevant with compression... so the best reason to do this is probably to avoid noise when reading the transpiled JS, and to make the code feel more idiomatic/familiar to the Rust dev.
// What about somthing like `impl MyTrait for Foo<Bar> { ... }` and `impl MyTrait for Foo<Baz> { ... }`. The methods should live on the Foo class, but the bodies/impls are different depending on the generic eg `let foo = Foo(Bar()).some_trait_method();` vs `let foo = Foo(Baz()).some_trait_method();`
// eg `let foo = Foo(Bar()).some_trait_method();` vs `let foo = Foo(Baz()).some_trait_method();`
// Or even just `impl Foo<Bar> { ... }` and `impl Foo<Baz> { ... }`.
// So need to monomorphize generic types that

// /// Match impl items to the classes in a `JsStmtModule`'s stmts and update the classes, recursively doing the same thing for any sub modules
// fn update_classes(
//     js_stmt_modules: &mut Vec<JsModule>,
//     // impl_items: &Vec<ImplItemTemp>,
//     impl_items: &Vec<RustImplBlock>,
//     default_trait_impls_class_mapping: &Vec<(String, String)>,
//     default_trait_impls: &Vec<(String, JsImplItem)>,
// ) {
//     let span = debug_span!("update_classes");
//     let _guard = span.enter();
//     // dbg!(&js_stmt_modules);
//     // dbg!(&impl_items);
//     for js_stmt_module in js_stmt_modules {
//         let span = debug_span!("update_classes for js_stmt_module", path = ?js_stmt_module.module_path, name = ?js_stmt_module.name);
//         let _guard = span.enter();

//         update_classes_js_stmts(
//             &mut js_stmt_module.stmts,
//             impl_items,
//             // default_trait_impls_class_mapping,
//             // default_trait_impls,
//         )
//     }
// }

fn get_traits_implemented_for_item(
    item_impls: &[RustImplBlockSimple],
    item_module_path: &[String],
    item_scope_id: &Option<Vec<usize>>,
    item_name: &str,
) -> Vec<(Vec<String>, Option<Vec<usize>>, String)> {
    // Does class implement the trait bounds of the impl block

    // TODO this code is duplicated from elsewhere
    // Each time we find new traits we need to again look for matching traits, and repeat this until we don't find any new traits
    let mut found_traits_count = 0;
    // (trait module path (None for scoped), trait name)
    let mut found_traits: Vec<(Vec<String>, Option<Vec<usize>>, String)> = Vec::new();
    loop {
        found_traits.clear();

        for item_impl in item_impls {
            // TODO this needs extending to handle matching any target type, rather than just user structs
            match &item_impl.target {
                RustType::TypeParam(rust_type_param) => {
                    // If the target is a type param then we must be implementing a trait
                    let rust_trait = item_impl.trait_.clone().unwrap();

                    // Get bounds on type param
                    let type_param_bounds = &item_impl
                        .generics
                        .iter()
                        .find(|generic| generic.ident == rust_type_param.name)
                        .unwrap()
                        .trait_bounds;

                    // Does our struct impl all of these traits?
                    let struct_impls_all_bounds = type_param_bounds
                        .iter()
                        .all(|type_param_bound| found_traits.contains(type_param_bound));
                    if struct_impls_all_bounds {
                        found_traits.push(rust_trait);
                    }
                }
                RustType::StructOrEnum(_, _, _, _) => {
                    if let Some(impl_trait) = &item_impl.trait_ {
                        let types_match = match &item_impl.target {
                            RustType::StructOrEnum(_type_params, module_path, scope_id, name) => {
                                module_path == item_module_path
                                    && scope_id == item_scope_id
                                    && name == item_name
                            }

                            _ => todo!(),
                        };

                        if types_match {
                            found_traits.push(impl_trait.clone());
                        }
                    }
                }
                RustType::MutRef(_) => todo!(),
                RustType::Ref(_) => todo!(),
                _ => todo!(),
            }
        }

        if found_traits_count == found_traits.len() {
            break;
        } else {
            found_traits_count = found_traits.len();
        }
    }

    found_traits
}

// // For impl blocks targetting a concrete type (including when implementing a trait), we want to add the items directly to the JS class, because no other types/classes are using the same implementation. For blocks targetting a generic type eg `Foo<T>` or `T`, we need to check if multiple classes are using the same method impls and if so keep the impl standalone and point to the impl from a class method.
// // IMPORTANT NOTE remember that when matching a struct/enum/class, we need more than just the item definition, we need it's scope/module path because we don't want to match any struct with the same name, we want to match the actual same definition so we need it's path.
// fn update_classes_js_stmts(
//     js_stmts: &mut Vec<JsStmt>,
//     impl_items: &Vec<RustImplBlock>,
//     // default_trait_impls_class_mapping: &Vec<(String, String)>,
//     // default_trait_impls: &Vec<(String, JsImplItem)>,
// ) {
//     for stmt in js_stmts {
//         match stmt {
//             JsStmt::Class(js_class) if !js_class.is_impl_block => {
//                 let span = debug_span!("update_classes for js_class", name = ?js_class.name);
//                 let _guard = span.enter();

//                 // First get the traits which the struct impls, to avoid doing it for every impl block
//                 let traits_impld_for_class = get_traits_implemented_for_item(
//                     impl_items,
//                     &js_class.module_path,
//                     js_class.sc
//                     &js_class.rust_name,
//                 );

//                 for impl_block in impl_items.clone() {
//                     // TODO impl could be in another module?
//                     match &impl_block.target {
//                         // add impl methods to class
//                         RustType::StructOrEnum(_, impl_target_module_path, impl_target_name) => {
//                             if &js_class.rust_name == impl_target_name
//                                 && &js_class.module_path == impl_target_module_path
//                             {
//                                 for impl_item in impl_block.items {
//                                     match impl_item.item {
//                                         RustImplItemItem::Fn(private, static_, fn_info, js_fn) => {
//                                             let FnInfo { ident, .. } = fn_info;

//                                             // If the struct is generic *and* already has a method with the same name as one in the impl block, then we need to monomorphize the struct
//                                             if js_class
//                                                 .methods
//                                                 .iter()
//                                                 .any(|method| method.0 == ident)
//                                             {
//                                                 todo!("need to monomorphize");
//                                             } else {
//                                                 js_class
//                                                     .methods
//                                                     .push((ident, private, static_, js_fn));
//                                             }
//                                         }
//                                         RustImplItemItem::Const(js_local) => {
//                                             js_class.static_fields.push(js_local);
//                                         }
//                                     }
//                                 }
//                             }
//                         }
//                         RustType::TypeParam(rust_type_param) => {
//                             // Get bounds on type param
//                             let type_param_bounds = &impl_block
//                                 .generics
//                                 .iter()
//                                 .find(|generic| generic.ident == rust_type_param.name)
//                                 .unwrap()
//                                 .trait_bounds;

//                             // Does our struct impl all of these traits?
//                             let struct_impls_all_bounds =
//                                 type_param_bounds.iter().all(|type_param_bound| {
//                                     traits_impld_for_class.contains(type_param_bound)
//                                 });

//                             if struct_impls_all_bounds {
//                                 for impl_item in &impl_block.items {
//                                     match &impl_item.item {
//                                         RustImplItemItem::Fn(private, static_, fn_info, js_fn) => {
//                                             let FnInfo { ident, .. } = fn_info;

//                                             // If the struct is generic *and* already has a method with the same name as one in the impl block, then we need to monomorphize the struct. TODO maybe it is better to check the need for monomorphization explicitly, in an early pass of process_items?

//                                             if js_class
//                                                 .methods
//                                                 .iter()
//                                                 .any(|method| &method.0 == ident)
//                                             {
//                                                 todo!("need to monomorphize");
//                                             } else {
//                                                 js_class.static_fields.push(JsLocal {
//                                                     public: false,
//                                                     export: false,
//                                                     type_: LocalType::None,
//                                                     lhs: LocalName::Single(camel(ident.clone())),
//                                                     value: JsExpr::Path(
//                                                         [
//                                                             impl_block.js_name(),
//                                                             "prototype".to_string(),
//                                                             camel(ident.clone()),
//                                                         ]
//                                                         .to_vec(),
//                                                     ),
//                                                 });
//                                             }
//                                         }
//                                         RustImplItemItem::Const(js_local) => {
//                                             // js_class.static_fields.push(js_local);
//                                             todo!()
//                                         }
//                                     }
//                                 }
//                             }
//                         }
//                         // IMPORTANT NOTE I did have a todo! here but it would without fail cause rust-analyzer to crash when I moved my cursor there, and took down vscode with it
//                         _ => {}
//                     }
//                 }

//                 // TODO when adding a default impl to a class, we need to know which module/scope it came from in case there are two trait with the same name
//                 // TODO also need to only add if there is not an impl which overrides the default
//                 // add trait methods with default impl to class
//                 // let trait_names = default_trait_impls_class_mapping.iter().filter_map(
//                 //     |(class_name, trait_name)| (class_name == &js_class.name).then_some(trait_name),
//                 // );
//                 // for trait_name in trait_names {
//                 //     let impl_items =
//                 //         default_trait_impls
//                 //             .iter()
//                 //             .filter_map(|(trait_name2, js_impl_item)| {
//                 //                 (trait_name == trait_name2).then_some(js_impl_item.clone())
//                 //             });
//                 //     for js_impl_item in impl_items {
//                 //         match js_impl_item {
//                 //             JsImplItem::ClassStatic(js_local) => {
//                 //                 js_class.static_fields.push(js_local);
//                 //             }
//                 //             JsImplItem::ClassMethod(name, private, static_, js_fn) => {
//                 //                 js_class.methods.push((name, private, static_, js_fn));
//                 //             }
//                 //             stmt => {
//                 //                 dbg!(stmt);
//                 //                 panic!("this JsStmt cannot be an impl item")
//                 //             }
//                 //         }
//                 //     }
//                 // }
//             }
//             // JsStmt::Module(js_stmt_module) => update_classes(js_stmt_module, impl_items.clone()),
//             _ => {}
//         }
//     }
// }

/// Populates `item_def.impl_blocks: Vec<String>` with ids of impl blocks
fn populate_item_def_impl_blocks(modules: &mut [ModuleData], impl_blocks: &[RustImplBlockSimple]) {
    let span = debug_span!("update_classes");
    let _guard = span.enter();

    // let impl_blocks = global_data.impl_blocks_simpl.clone();
    // First update item defs in modules, then update the rust prelude item defs which are global and don't sit in a module
    // for module in &mut global_data.modules {
    for module in modules {
        // dbg!(&module.parent_name);
        // let clone_scoped_various_definitions = module.scoped_various_definitions.clone();
        // let scoped_impl_blocks = clone_scoped_various_definitions
        //     .iter()
        //     .map(|svd| &svd.2)
        //     .flatten();

        // IMPORTANT TODO all this needs improving, especially to ensure we are only trying to match scoped impls that can actually reach the item. Need unit tests.

        let scoped_item_defs = module
            .scoped_various_definitions
            .iter_mut()
            .flat_map(|svd| {
                svd.1
                    .item_definitons
                    .iter_mut()
                    .map(|item_def| (item_def, Some(svd.0.clone())))
            });
        let module_item_defs = module
            .various_definitions
            .item_definitons
            .iter_mut()
            .map(|item_def| (item_def, None));

        // module level items/classes
        for (item_def, item_def_scope_id) in scoped_item_defs.chain(module_item_defs) {
            // dbg!(&item_def);
            update_item_def_block_ids(
                item_def,
                &item_def_scope_id,
                &module.path.clone(),
                impl_blocks,
            );
        }
    }

    // let prelude_module = global_data
    //     .modules
    //     .iter_mut()
    //     .find(|m| m.path == [PRELUDE_MODULE_PATH])
    //     .unwrap();
    // // dbg!(&prelude_module);
    // for item_def in prelude_module
    //     .item_definitons
    //     .iter_mut()
    //     .filter(|item_def| item_def.ident != "Box")
    // {
    //     update_item_def_block_ids(item_def, &None, &vec!["donotuse".to_string()], &impl_blocks);
    // }
}

fn update_item_def_block_ids(
    item_def: &mut ItemDefinition,
    item_def_scope_id: &Option<Vec<usize>>,
    module_path: &[String],
    // global_data: &GlobalData,
    impl_blocks: &[RustImplBlockSimple],
) {
    let traits_impld_for_class =
        get_traits_implemented_for_item(impl_blocks, module_path, &None, &item_def.ident);
    // for impl_block in impl_blocks.iter().chain(scoped_impl_blocks.clone()) {
    for impl_block in impl_blocks {
        // NOTE we differentiate between concrete and type param targets because for (non-generic TODO) concrete types we only have to match on item name/id, whereas for type params we have to check if the item implements all the type bounds
        match &impl_block.target {
            // Concrete type target
            RustType::StructOrEnum(
                _,
                impl_target_module_path,
                impl_target_scope_id,
                impl_target_name,
            ) => {
                if &item_def.ident == impl_target_name
                    && module_path == impl_target_module_path
                    && item_def_scope_id == impl_target_scope_id
                {
                    // The purpose of storing this info on the item_def is so that after the syn -> JS parsing parsing has happened and we have a parsed impl block and items, we can use this info to lookup this parsed impl block and copy it's methods/fields to the class.
                    // item_def
                    //     .impl_blocks
                    //     .push(ItemDefintionImpls::ConcreteImpl(impl_block.items.clone()));
                    item_def.impl_block_ids.push(impl_block.unique_id.clone());
                }
            }
            // For a generic impl like `impl<T> Foo for T {}`, remember this will add methods to matching items in *all* scopes, both parent and children, the only restriction is the the `impl`d trait must be in accessible/in scope *when the method is called*
            RustType::TypeParam(rust_type_param) => {
                // Get bounds on type param
                let type_param_bounds = &impl_block
                    .generics
                    .iter()
                    .find(|generic| generic.ident == rust_type_param.name)
                    .unwrap()
                    .trait_bounds;

                // Does our struct impl all of these traits?
                let struct_impls_all_bounds = type_param_bounds
                    .iter()
                    .all(|type_param_bound| traits_impld_for_class.contains(type_param_bound));

                // TODO we might be adding this impl to items that meet the trait bound, but methods from the impl'd trait are not acutally called. There doesn't seem to be any easy, direct way to check this, the best approach is possibly for each method stored on the `ItemDefinition` to have a flag which get's set to true when the method is called/looked up, and otherwise remains false and is not used/output when the item/class is written to JS.

                if struct_impls_all_bounds {
                    item_def.impl_block_ids.push(impl_block.unique_id.clone());
                }
            }
            // IMPORTANT NOTE I did have a todo! here but it would without fail cause rust-analyzer to crash when I moved my cursor there, and take down vscode with it
            _ => {}
        }
    }
}

// TODO needs to be able to distinguish between `web_prelude` which is being using as a third party crate and something that has been defined in the code, ie I think any time we find a `web_prelude` we need to check if there is any user defined item or var with the same name in scope
fn look_for_web_prelude(modules: &Vec<ModuleDataFirstPass>) -> bool {
    let mut found_web_prelude = false;
    for module in modules {
        for item in &module.items {
            match item {
                Item::Const(_) => {}
                Item::Enum(_) => {}
                Item::ExternCrate(_) => {}
                Item::Fn(_) => {}
                Item::ForeignMod(_) => {}
                Item::Impl(_) => {}
                Item::Macro(_) => {}
                Item::Mod(_) => {}
                Item::Static(_) => {}
                Item::Struct(_) => {}
                Item::Trait(_) => {}
                Item::TraitAlias(_) => {}
                Item::Type(_) => {}
                Item::Union(_) => {}
                Item::Use(item_use) => match &item_use.tree {
                    UseTree::Path(use_path) => found_web_prelude = use_path.ident == "web_prelude",
                    UseTree::Name(use_name) => found_web_prelude = use_name.ident == "web_prelude",
                    UseTree::Rename(_) => {}
                    UseTree::Glob(_) => {}
                    UseTree::Group(_) => {}
                },
                Item::Verbatim(_) => {}
                _ => {}
            }
        }
    }
    found_web_prelude
}

fn push_rust_types(global_data: &GlobalData, js_stmts: &mut Vec<JsStmt>) {
    // We want to insert prelude stmts at the beginning of js_stmts, but if we do that per item we will reverse the order they appear in the source files. Instead we push them to `prelude_stmts` and then insert that in one go
    let mut prelude_stmts = Vec::new();

    let rust_prelude_types = &global_data.rust_prelude_types;

    if rust_prelude_types.rust_integer {
        let js_class = JsStmt::Class(JsClass {
            public: false,
            export: false,
            tuple_struct: false,
            name: Ident::Str("RustInteger"),
            inputs: vec![Ident::Str("inner")],
            static_fields: Vec::new(),
            methods: Vec::new(),
            rust_name: "donotuse".to_string(),
            is_impl_block: false,
            module_path: vec!["rust_std_prelude".to_string()],
            scope_id: None,
        });
        prelude_stmts.push(js_class);
    }
    if rust_prelude_types.rust_string {
        let js_class = JsStmt::Class(JsClass {
            public: false,
            export: false,
            tuple_struct: false,
            name: Ident::Str("RustString"),
            inputs: vec![Ident::Str("inner")],
            static_fields: Vec::new(),
            methods: Vec::new(),
            rust_name: "donotuse".to_string(),
            is_impl_block: false,
            module_path: vec!["rust_std_prelude".to_string()],
            scope_id: None,
        });
        prelude_stmts.push(js_class);
    }
    if rust_prelude_types.rust_array_copy {
        let js_stmt = JsStmt::Raw(
            r"
                Array.prototype.copy = function () {
                    return JSON.parse(JSON.stringify(this));
                };
            "
            .to_string(),
        );
        prelude_stmts.push(js_stmt);
    }

    if rust_prelude_types.option_is_some_and {
        let js_stmt = JsStmt::Raw(
            "function optionIsSomeAnd(self, f) {
                return self !== null ? f(self) : false;
            }"
            .to_string(),
        );
        prelude_stmts.push(js_stmt);
    }

    if rust_prelude_types.option_unwrap {
        let js_stmt = JsStmt::Raw(
            r#"function optionUnwrap(self) {
                if (self === null) {
                    throw new Error("called `Option::unwrap()` on a `None` value");
                } else {
                    return self;
                }
            }"#
            .to_string(),
        );
        prelude_stmts.push(js_stmt);
    }

    if rust_prelude_types.vec {
        let methods = [
            (
                Ident::Str("new"),
                true,
                JsFn {
                    iife: false,
                    export: false,
                    public: false,
                    async_: false,
                    is_method: true,
                    name: Ident::Str("new"),
                    input_names: Vec::new(),
                    body_stmts: vec![JsStmt::Raw("this.vec = [];".to_string())],
                },
            ),
            (
                Ident::Str("push"),
                false,
                JsFn {
                    iife: false,
                    export: false,
                    public: false,
                    async_: false,
                    is_method: true,
                    name: Ident::Str("push"),
                    input_names: vec![Ident::Str("elem")],
                    body_stmts: vec![JsStmt::Raw("this.vec.push(elem);".to_string())],
                },
            ),
        ]
        .to_vec();
        prelude_stmts.push(JsStmt::Class(JsClass {
            export: false,
            public: false,
            name: Ident::Str("Vec"),
            tuple_struct: false,
            inputs: Vec::new(),
            static_fields: Vec::new(),
            methods,
            rust_name: "donotuse".to_string(),
            is_impl_block: false,
            module_path: vec!["rust_std_prelude".to_string()],
            scope_id: None,
        }));
    }

    if rust_prelude_types.option {
        // let code = include_str!("rust_prelude/option.rs");
        // let modules = from_file(code, false);
        // assert_eq!(modules.len(), 1);
        // let option_module = &modules[0];

        // for stmt in &option_module.stmts {
        //     match stmt {
        //         JsStmt::Class(js_class) => {
        //             if js_class.name == "Option" {
        //                 prelude_stmts.push(stmt.clone());
        //             }
        //         }
        //         JsStmt::ClassMethod(_, _, _, _) => todo!(),
        //         JsStmt::ClassStatic(_) => todo!(),
        //         // JsStmt::Local(js_local) => match &js_local.lhs {
        //         //     LocalName::Single(name) => {
        //         //         if name == "Some" || name == "None" {
        //         //             js_stmts.insert(0, stmt.clone());
        //         //         }
        //         //     }
        //         //     LocalName::DestructureObject(_) => todo!(),
        //         //     LocalName::DestructureArray(_) => todo!(),
        //         // },
        //         JsStmt::Local(_) => todo!(),
        //         JsStmt::Expr(_, _) => todo!(),
        //         JsStmt::Import(_, _, _) => todo!(),
        //         JsStmt::Function(_) => todo!(),
        //         JsStmt::ScopeBlock(_) => todo!(),
        //         // JsStmt::TryBlock(_) => todo!(),
        //         // JsStmt::CatchBlock(_, _) => todo!(),
        //         JsStmt::Raw(_) => todo!(),
        //         JsStmt::Comment(_) => todo!(),
        //     }
        // }
    }
    if rust_prelude_types.some {
        // prelude_stmts.push(JsStmt::Raw("let Some = Option.Some;".to_string()))
    }
    if rust_prelude_types.none {
        // prelude_stmts.push(JsStmt::Raw("let None = Option.None;".to_string()))
    }

    if rust_prelude_types.string_prototype_extensions {
        prelude_stmts.push(JsStmt::Raw(
            include_str!("../../ravascript/tests/string_prototype_extensions.js").to_string(),
        ))
    }
    if rust_prelude_types.number_prototype_extensions {
        prelude_stmts.push(JsStmt::Raw(
            include_str!("../../ravascript/tests/number_prototype_extensions.js").to_string(),
        ))
    }

    if rust_prelude_types.integer || rust_prelude_types.float {
        let code = include_str!("rust_prelude/number.rs");
        let modules = from_file(code, false);
        assert_eq!(modules.len(), 1);
        let number_module = &modules[0];

        for stmt in &number_module.stmts {
            match stmt {
                JsStmt::Class(js_class) => {
                    if js_class.name == "RustInteger" {
                        if rust_prelude_types.integer {
                            prelude_stmts.push(stmt.clone());
                        }
                    } else if js_class.name == "RustFloat" {
                        if rust_prelude_types.float {
                            prelude_stmts.push(stmt.clone());
                        }
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        }
    }

    if rust_prelude_types.string {
        let code = include_str!("rust_prelude/string.rs");
        let modules = from_file(code, false);
        assert_eq!(modules.len(), 1);
        let string_module = &modules[0];

        for stmt in &string_module.stmts {
            match stmt {
                JsStmt::Class(js_class) => {
                    // TODO don't need this if check?
                    if js_class.name == "RustString" {
                        prelude_stmts.push(stmt.clone());
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        }
    }

    if rust_prelude_types.bool {
        let code = include_str!("rust_prelude/bool.rs");
        let modules = from_file(code, false);
        assert_eq!(modules.len(), 1);
        let bool_module = &modules[0];

        for stmt in &bool_module.stmts {
            match stmt {
                JsStmt::Class(js_class) => {
                    if js_class.name == "RustBool" {
                        prelude_stmts.push(stmt.clone());
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        }
    }

    if rust_prelude_types.equals {
        // TODO write the function in Rust and transpile it
        prelude_stmts.push(JsStmt::Raw(
            "function eq(lhs, rhs) { return JSON.stringify(lhs) === JSON.stringify(rhs); }"
                .to_string(),
        ))
    }

    // Add prelude_stmts to beginning of js_stmts
    prelude_stmts.append(js_stmts);
    js_stmts.append(&mut prelude_stmts);
}

pub fn process_items(
    items: Vec<Item>,
    crate_path: Option<PathBuf>,
    // TODO I don't think there is much point in supporting generation without "Rust types" so remove this flag
    with_rust_types: bool,
    // We use this to know whether we should insert eg
    is_block: bool,
) -> Vec<JsModule> {
    let mut modules = Vec::new();
    // let mut crate_module = ModuleData::new("crate".to_string(), None, vec!["crate".to_string()]);
    let mut crate_module = ModuleDataFirstPass::new("crate".to_string(), vec!["crate".to_string()]);
    crate_module.items.clone_from(&items);
    modules.push(crate_module);

    // gets names of module level items, creates `ModuleData` for each sub module, and adds `use` data to module's `.use_mapping`
    extract_modules(
        true,
        &items,
        &crate_path,
        &mut vec!["crate".to_string()],
        &mut modules,
    );

    // TODO web prelude should probably be a cargo-js/ravascript module, not an entire crate? If people are going to have to add ravascript as a dependency as well as install the CLI then yes, otherwise if they have no dependency to add other than the web prelude, may as well make it a specific crate?
    // Now that with have extracted data for the main.rs/lib.rs, we do the same for third party crates.
    // Currently this is only the web prelude for which we need to `extract_data` for deduplicating/namespacing and getting use mappings for resolving paths, and `extract_data_populate_item_definitions` for getting item definitions to eg lookup methods etc, but we do not actually need to parse to a JS AST.

    // NOTE We process the web-prelude module in order that we can find it's item definitions, etc, and its names are taken into account when deduplicating/qualifying idents, but we don't parse it because we obviously don't actually need it in the output because it all already exists in the browser enivronment/prelude.
    // We would ideally determine third party crates to include by checking the the Cargo.toml, as simply checking use stmts is harder and even though they should RA/cargo-check error if the crate isn't actually included. The problem is that eg for testing using from_block and frm_file, there is no Cargo.toml. We could add a `use_web: bool` flag argument to these functions, but that seems verbose when the main use case is for the web (we can just have different fns like `from_block_web` and `from_block` to avoid verboseness) (though in fairness if being used for library stuff it might not directly need web-prelude types?).
    // Options:
    // 1. flag argument
    // 2. always include web-prelude
    // 3. use Cargo.toml for all third party crates except web-prelude and do a search of `Item::Use`s for "web-prelude". Remember you don't actually have to `use` an external crate, you can just add it to Cargo.toml and use it directly in a path like `let div = web_prelude::create_element("div");`

    // 2. Is the simplest and cleanest approach, the only real drawback is it is a bit more of a "special case"ing approach, and is annoying when trying to debug stuff that doesn't use
    // Even though RA won't work, we do still want to support cases using from_block etc with no Cargo.toml because it is useful for integrating/embedding into applications eg for our online convertor there will be no Cargo.toml or proper rust project, effectively just a String of Rust code, but we still want to be able to write code using web-prelude and have it transpile, and the web-prelude code to be available (so needs to be `include_string`ed) so that it is taken into account when name deduping. Also transpiling a specific file with the CLI is another use case. We probably actually want to support transpiling an individual file which is part of a Rust project and still actually uses third party crates, does all the same name dedup etc (maybe?) but only outputs the contents of that single file as that would be useful for debugging? And remember that although it seems useful to transpile random .rs files, eg could just add a single .rs file in a js project, that is going to be quite atypical since that would require writing a .rs file without RA.
    // Remember even just being used in from_block we still need web_prelude in our Cargo.toml - yes if being used in a Rust project but not in other embedded situtations.
    // So a flag seems the best approach but would be even easier to just check it using `Item::Use` so that is one less things for users to worry about or get wrong, yeah but flag is easier just just use that for now (especially given it is a niche use case), to check for use of web_prelude we need to resolve all the use stmts to make sure it isn't acutally eg `mod web_prelude { user stuff ... }`

    // Look for web prelude
    // TODO make this correct. Whilst it would be easier to identify web_prelude usage if `make_use_mappings_absolute()` (or whatever) was implemented, it is still impossible to know whether web_prelude is being used until we parse the entire syntax in `js_stmts_from_syn_items()` since we can have cases like:
    // `let div = web_prelude::document::create_element("div");`
    let include_web = look_for_web_prelude(&modules);

    if include_web {
        let web_prelude_crate_path = "../web-prelude";
        // We include the web prelude at compile time so that it can be used for eg from_block or from_file which operate on simple strings of Rust code and no Cargo project
        // let code = fs::read_to_string(web_prelude_entry_point_path).unwrap();
        let code = include_str!("../../web-prelude/src/lib.rs");
        let file = syn::parse_file(code).unwrap();
        let prelude_items = file.items;
        let mut module_data =
            ModuleDataFirstPass::new("web_prelude".to_string(), vec!["web_prelude".to_string()]);
        module_data.items.clone_from(&prelude_items);
        modules.push(module_data);

        extract_modules(
            true,
            &prelude_items,
            &Some(web_prelude_crate_path.into()),
            &mut vec!["web_prelude".to_string()],
            &mut modules,
        );
    };

    // Rust prelude
    let code = include_str!("rust_prelude/option.rs");
    let file = syn::parse_file(code).unwrap();
    let prelude_items = file.items;
    let mut module_data = ModuleDataFirstPass::new(
        PRELUDE_MODULE_PATH.to_string(),
        vec![PRELUDE_MODULE_PATH.to_string()],
    );

    module_data.items.clone_from(&prelude_items);
    modules.push(module_data);

    extract_modules(
        true,
        &prelude_items,
        // TODO for now use None since we are using a single file but probably want to eventually expand to some kind of fake "lib"
        &None,
        &mut vec![PRELUDE_MODULE_PATH.to_string()],
        &mut modules,
    );

    // TODO convert relative_path use_mappings to absolute paths to simply `resolve_path` and prevent duplicate work? `resolve_path` already nicely handles this so would be duplicating code?

    // In extract_data() we record all module level item/fn/trait definitions/data in the `ModulData`. However, the types used in the definition might be paths to an item/fn that hasn't been parsed yet. This means we need to either:
    // 1. Only use syn for resolving types. Currently we use `get_path()` to resolve paths which uses the parsed `ItemDefinition`s etc, we could update it/make a different version to work directly on syn data, but this seems like it would be inefficient.
    // 2. Do another pass. Leave the types empty in the definitions for `extract_data()` and then do another pass to populate them once we have all the item defs etc so `get_path()` will work.
    // NOTE `get_path()` shouldn't need full `ItemDefinition`s etc, it should only need the names of each item in each module. Given having partially completed `ItemDefinition`s etc will be a pain without using Option<T> or something (eg FnInfo has `return_type: RustType`, though could just use eg RustType::Todo or something), maybe we should update `get_path()` to only use eg ModuleData.pub_definitions instead of `ItemDefinition`s... NO:
    // So `get_path()` doesn't actually use `ItemDefinition`s etc, only ModuleData.pub_definitions etc...
    // So we can just entirely populate .item_definitions etc in the second pass, as parse_input... whatever is not using item_definitions...

    // So, we are using `parse_fn_input_or_field()` to parse: struct fields, and probably more stuff when finished like fn inputs and return, maybe trait bound?, etc
    // `parse_fn_input_or_field()` currently uses `ItemDefinition`s etc, which doesn't work because that is what we are in the process of creating... however the only thing it uses from the `ItemDefinition` is the names of the genereics ie:
    // let gen_arg_name = item_definition.generics[i].clone();
    //
    // Also populates `global_data.impl_blocks` so that in the next step, before parsing the syn to JS, we can populate `item_definition.impl_items`, so that when parsing syn to JS we are able to to lookup return types of method calls, and also add the methods themselves to the JS classes
    // ie populate module and scoped `fn_info`, `item_definitons`, `consts`, `trait_definitons`.
    // We need to only populate the idents of the `ItemDefinition`s etc here, and then in a subsequent pass populate all the fields which use types and therefore might require other `ItemDefinition`s to already exist even though they may appear later in the code.

    // Updates module.item_defs etc and module.scoped_various_definitions. Doesn't use any data from global_data, only pushes data to ModuleData
    let mut actual_modules = make_item_definitions(modules);

    // Need to manually add the Fn traits because we can't redefine them to allow them be read in with all the prelude items.
    let prelude_module_data = actual_modules
        .iter_mut()
        .find(|m| m.path == [PRELUDE_MODULE_PATH])
        .unwrap();
    prelude_module_data
        .various_definitions
        .trait_definitons
        .push(make_item_definitions::RustTraitDefinition {
            name: "FnOnce".to_string(),
            is_pub: true,
            syn: syn::parse_str::<ItemTrait>("trait FnOnce {}").unwrap(),
        });
    prelude_module_data
        .various_definitions
        .trait_definitons
        .push(make_item_definitions::RustTraitDefinition {
            name: "Copy".to_string(),
            is_pub: true,
            syn: syn::parse_str::<ItemTrait>("trait Copy {}").unwrap(),
        });

    // populates `global_data.impl_blocks_simpl` and defs that use types like a structs fields in it's ItemDef, fn arguments, etc
    // TODO re updating item defs here because we need to be able to lookup other types used in item defs which might appear later: if we update extract_data to gather the location of items, rather than just their idents, we could use that data and do it all in populate_item_definitions rather than needing to do some here... although that does mean we would need to start tracking the scope in `extract_data` which we currently don't need to so that seems suboptimal
    let (mut new_modules, impl_blocks) = update_item_definitions(actual_modules);

    // global_data_crate_path is use when reading module files eg global_data_crate_path = "../my_crate/" which is used to prepend "src/some_module/submodule.rs"

    // Match `RustImplBlockSimpl`s to item definitions. It is necessary to do it at this stage so that we can look up method info when parsing syn -> JS. We also use this in update_classes2 to know which parsed JS impls to lookup to add their methods/fields to the class. What??? there doesn't seem to be any JS parsing here?
    // iterates through `global_data.impl_blocks_simpl`'s `RustImplBlockSimple`s to populate `item_def.impl_blocks` with `ItemDefintionImpls`s
    // TODO need to also look through the scoped `RustImplBlockSimple` and populate either scoped *or* module level `item_def.impl_blocks`s with `ItemDefintionImpls`s
    // Populates `item_def.impl_blocks: Vec<String>` with ids of impl blocks
    populate_item_def_impl_blocks(&mut new_modules, &impl_blocks);

    // It makes sense to add impl block items/methods to items/classes at this point since methods and classes are completely static (definitions) and not impacted by runtime info like the instances of the items and their resolved type params, rather than doing it later where we are working with `JsStmt`s and have lost the info about which item it is (ie we no longer have the module path of the item, just the duplicated JS name). But the most important reason is that we need to be able to add methods to module level items/class when we encounter a scoped impl, and `JsClass`s might not exist for all the items/classes at that point.
    // This does mean we need a way of storing info about methods on the item definitions, that is then used for creating the JsClass along with methods... but if we are updating the item definitions during `js_stmts_from_syn_items` then the methods specified on an item definition when the item is parsed to a class might not yet contain the total final number of methods once all the scoped impls have been parsed!!!! Just have to split this into 2 separate passes? But this means remembering the location of scoped items, by either creating a whole new AST which contains both syn and item definitions, which seems overkill, or finding a way to assign a unique id to scoped items eg using the number of the scope.
    // Or... we can do the JS parsing first, retain the item definition for each JsClass produced... and *then* handle matching of scoped items and impl blocks

    // We will however still need to also do the same/similar handling for scope items or impl blocks in js_stmts_from_syn_items, specifically, at the end of any scope (technically we only need to if a new item definition or impl block has been added), we must:
    // 1. If we have a new item definition in the scope, iterate through every impl block, both module level impl blocks, all of which will already exist, and all the impl blocks in the scopes, and check whether any of them have impld items which should be added to the item definition.
    // 2. If we a have a new impl block in the scope, if it is a trait impl we have to check *everything* again because it might for example impl a new trait for a module level struct, which means that now a different impl block might match the struct, so the methods from that block need to be added to the struct item definition and so on. For example:
    {
        // module top level
        trait Foo {
            fn get_foo(&self) -> i32 {
                4
            }
        }
        struct Bar {}
        {
            impl<T> Foo for T {}
        }
        let bar = Bar {};
        bar.get_foo();
    }

    // find duplicates
    let duplicates = namespace_duplicates(&new_modules);

    let mut global_data = GlobalData::new(crate_path);
    global_data.modules = new_modules;
    global_data.impl_blocks_simpl = impl_blocks;
    global_data.duplicates = duplicates;

    // Parse to JS
    for module_data in global_data
        .modules
        .clone()
        .into_iter()
        .filter(|m| m.path != ["web_prelude"] && m.path != [PRELUDE_MODULE_PATH])
    {
        global_data.scope_count.clear();
        global_data.scope_count.push(0);
        global_data.scope_id.clear();
        global_data.scopes.clear();
        let mut stmts =
            js_stmts_from_syn_items(module_data.items, &module_data.path, &mut global_data);

        if with_rust_types && module_data.path == ["crate"] {
            let stmts = if is_block {
                assert_eq!(stmts.len(), 1);
                match stmts.first_mut().unwrap() {
                    JsStmt::Function(js_fn) if js_fn.name == "temp" => &mut js_fn.body_stmts,
                    other => {
                        dbg!(other);
                        todo!()
                    }
                }
            } else {
                &mut stmts
            };
            push_rust_types(&global_data, stmts);
        };

        global_data.transpiled_modules.push(JsModule {
            public: true,
            name: Ident::String(module_data.name.clone()),
            module_path: module_data.path.clone(),
            stmts,
        });
    }

    // global_data.transpiled_modules.push(JsModule {
    //     public: true,
    //     name: "crate".to_string(),
    //     module_path: vec!["crate".to_string()],
    //     stmts: Vec::new(),
    // });

    // NOTE IMPORTANT item impls are populated in js_stmts_from_syn_items, which we don't run for web_prelude, which means create_element method is not found
    // let stmts = js_stmts_from_syn_items(items, &mut vec!["crate".to_string()], &mut global_data);

    // TODO IMPORTANT this doesn't seem to make sense. `js_stmts_from_syn_items` starts with the "crate" module items, but then reads the items for each module it encounters, so `stmts` contains the entire crates items, but ten we are adding it to the `crate_module.stmts`, so all the other module's `.stmt`s will be empty. We could handle the `Item`s for each module separately since we do actually store them on `ModuleData`, which would mean we don't have to save the `GlobalData` scope state between modules, but it would be better to avoid storing syn `Item`s on `ModuleData` anyway because it is not good for testing. How does the module name comments added below work then since they are added to `.stmts`??
    // let crate_module = global_data
    //     .transpiled_modules
    //     .iter_mut()
    //     .find(|tm| tm.module_path == vec!["crate".to_string()])
    //     .unwrap();
    // crate_module.stmts = stmts;

    // 5 failed in test "impl" when commenting out update_classes
    let global_data_copy = global_data.clone();
    update_classes2(&mut global_data.transpiled_modules, &global_data_copy);

    // For impl blocks which target Rust prelude types eg `impl i32`, `impl<T> Foo for T`, etc, append their js classes with any prototype extensions that need to be applied to JS primatives eg `Number.prototype.getFoo = FooForT.prototype.getFoo` (note these are just assignment statments so won't be hoisted and can't appear just anywhere, the must appear *after* the impl block's class).
    // Actually should do this at the point the impl block is parsed?
    // for item_def in &global_data.rust_prelude_definitions {}

    // resolve paths to get canonical path to item
    // Update paths to use namespaced names and account for `use` statements
    // We want to find the module where the item is actually defined, not where it is mod, use, pub from other modules
    // Solution:
    // x First find all the items defined in each module
    // Then for each module, add maps of the names of the items that are mod/use'd to the use path
    // go through each module and reconcile each item with the use mapping, and that with the module path where the item is actually defined

    // Remember that use might only be `use`ing a module, and then completing the path to the actual item in the code. So the final step of reconciliation will always need make use of the actual paths/items in the code
    // dbg!(global_data.modules);

    // TODO can this not just be done automatically in JsModule.js_string() ??
    // add module name comments when there is more than 1 module

    // for m in &global_data.transpiled_modules {
    //     dbg!(&m.module_path);
    //     dbg!(&m.name);
    //     dbg!(&m.stmts.len());
    // }
    if global_data.transpiled_modules.len() > 1 {
        for module in global_data
            .transpiled_modules
            .iter_mut()
            .filter(|m| m.module_path != ["web_prelude"])
        {
            module.stmts.insert(
                0,
                JsStmt::Comment(if module.module_path == ["crate"] {
                    "crate".to_string()
                } else {
                    module
                        .module_path
                        .iter()
                        .skip(1)
                        .cloned()
                        .collect::<Vec<_>>()
                        .join("::")
                }),
            );
        }
    }

    global_data.transpiled_modules
}

fn update_classes2(js_stmt_modules: &mut Vec<JsModule>, global_data: &GlobalData) {
    for js_module in js_stmt_modules {
        let _module = global_data
            .modules
            .iter()
            .find(|m| m.path == js_module.module_path)
            .unwrap();
        update_classes_stmts(&mut js_module.stmts, global_data);
    }
}

fn update_classes_stmts(js_stmts: &mut Vec<JsStmt>, global_data: &GlobalData) {
    for stmt in js_stmts {
        //
        match stmt {
            // JsStmt::Class(js_class) if !js_class.is_impl_block => {
            JsStmt::Function(js_fn) => {
                update_classes_stmts(&mut js_fn.body_stmts, global_data);
            }
            // TODO js_class has a `is_impl_block` field we should use here
            JsStmt::Class(js_class)
                if js_class.rust_name != "implblockdonotuse"
                    && js_class.rust_name != "donotuse" =>
            {
                let item_def = global_data.lookup_item_def_known_module_assert_not_func2(
                    &js_class.module_path,
                    &js_class.scope_id,
                    &js_class.rust_name,
                );
                // PROBLEM cant't just store the impl block id on the item def since if there are two impl blocks with the same signature we will just grab the first one twice. Need to either dedupe `global_data.impl_blocks`, store the names of the methods as a second id on the item def so we can tell them apart in `global_data.impl_blocks`, or find a better solution. Maybe we rather than loop through classes and then looking up impls, we can loop through the impls and look up classes? That way we wouldn't even need to store the impl block ids on the item defs. For now solve it with a hack and then do a proper refactor in a clean commit. Or dedup the ids in item_def.impl_blocks which is much easier then get multiple js_impl_blocks from global_data.impl_blocks.
                let mut dedup_impl_block_ids = item_def.impl_block_ids.clone();
                dedup_impl_block_ids.sort();
                dedup_impl_block_ids.dedup();
                for impl_block_id in &dedup_impl_block_ids {
                    // dbg!(&global_data.impl_blocks_simpl);
                    // dbg!(impl_block_id);
                    // let module_rust_impl_block = global_data
                    //     .impl_blocks_simpl
                    //     .iter()
                    //     .find(|rib| &rib.unique_id == impl_block_id)
                    //     .unwrap();
                    // let scoped_rust_impl_block = global_data
                    //     .s
                    //     .impl_blocks_simpl
                    //     .iter()
                    //     .find(|rib| &rib.unique_id == impl_block_id)
                    //     .unwrap();
                    for js_impl_block in global_data
                        .impl_blocks
                        .iter()
                        .filter(|jib| &jib.unique_id == impl_block_id)
                    {
                        let is_generic_impl =
                            matches!(js_impl_block.target, RustType::TypeParam(_));

                        for (_used, impl_item) in &js_impl_block.items {
                            // TODO implement used
                            // TODO What about `impl Foo for T {}`? This means we need to add prototype fields, not methods?
                            match &impl_item.item {
                                RustImplItemItemJs::Fn(static_, _fn_info, js_fn) => {
                                    if is_generic_impl {
                                        js_class.static_fields.push(JsLocal {
                                            public: false,
                                            export: false,
                                            type_: LocalType::None,
                                            lhs: LocalName::Single(js_fn.name.clone()),
                                            value: JsExpr::Path(PathIdent::Path(
                                                [
                                                    js_impl_block.js_name(),
                                                    Ident::Str("prototype"),
                                                    js_fn.name.clone(),
                                                ]
                                                .to_vec(),
                                            )),
                                        });
                                    } else {
                                        js_class.methods.push((
                                            Ident::String(item_def.ident.clone()),
                                            *static_,
                                            js_fn.clone(),
                                        ));
                                    }
                                }
                                RustImplItemItemJs::Const(_) => todo!(),
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

// /// Called from handle_item_struct and handle_item_enum, for the given item_def, loop through it's matched impl blocks, convert the method/const/etc to JS and push that to the methods/static_fields &mut Vec
// ///
// /// NOTE the big problem with this is that we are doing the syn -> JS parsing for the methods/static_fields out of order from original tree, so we will be calculating the scope id for handle_impl_item_fn incorrectly.
// /// For GenericImpl this doesn't seem t be a problem because we are grabbing the JS from `global_data.impl_blocks`.
// /// We can't just because a struct which needs the impl method can appear before the method.
// /// I believe we can just do an `update_js_classes` type operation after all the JS parsing, since now we are using scope_id's it will actually only be for the JS, and whereas before this would could problems when trying to lookup methods before the impl has been parsed, now we (or should) preprocess the impls and add the requried method info to the item definitions before the JS parsing
// ///
// /// -> (methods, static_fields)
// fn populate_fields_and_methods(
//     global_data: &mut GlobalData,
//     current_module_path: &Vec<String>,
//     // Remember JsClass expects JS names but class names are identical for Rust and JS
//     // item_name: String,
//     item_def: &ItemDefinition,
//     generics_type_params: &Vec<RustTypeParam>,
//     methods: &mut Vec<(String, bool, bool, JsFn)>,
//     static_fields: &mut Vec<JsLocal>,
// ) {
//     for impl_blocky in &item_def.impl_blocks {
//         match impl_blocky {
//             ItemDefintionImpls::GenericImpl(unique_name, method_name) => {
//                 // Find impl block
//                 // TODO this should be filter because there might be multiple impl blocks with the same "signature"
//                 let impl_block = global_data
//                     .impl_blocks
//                     .iter()
//                     .find(|impl_block| &impl_block.js_name() == unique_name)
//                     .unwrap();
//                 for rust_impl_item in &impl_block.items {
//                     match &rust_impl_item.item {
//                         RustImplItemItem::Fn(private, static_, fn_info, js_fn) => {
//                             methods.push((
//                                 item_def.ident.clone(),
//                                 *private,
//                                 *static_,
//                                 js_fn.clone(),
//                             ));
//                         }
//                         RustImplItemItem::Const(_) => todo!(),
//                     }
//                 }
//             }
//             ItemDefintionImpls::ConcreteImpl(impl_items) => {
//                 for impl_item in impl_items {
//                     match impl_item {
//                         ImplItem::Const(_) => todo!(),
//                         ImplItem::Fn(impl_item_fn) => {
//                             let mut rust_impl_items = Vec::new();
//                             let target_rust_type = RustType::StructOrEnum(
//                                 generics_type_params.clone(),
//                                 current_module_path.clone(),
//                                 global_data.scope_id_as_option(),
//                                 item_def.ident.clone(),
//                             );

//                             handle_impl_item_fn(
//                                 &mut rust_impl_items,
//                                 impl_item,
//                                 impl_item_fn,
//                                 global_data,
//                                 current_module_path,
//                                 &target_rust_type,
//                             );
//                             assert!(rust_impl_items.len() == 1);
//                             let rust_impl_item = rust_impl_items.remove(0);
//                             match &rust_impl_item.item {
//                                 RustImplItemItem::Fn(private, static_, fn_info, js_fn) => {
//                                     methods.push((
//                                         item_def.ident.clone(),
//                                         *private,
//                                         *static_,
//                                         js_fn.clone(),
//                                     ));
//                                 }
//                                 RustImplItemItem::Const(_) => todo!(),
//                             }
//                         }
//                         ImplItem::Type(_) => todo!(),
//                         ImplItem::Macro(_) => todo!(),
//                         ImplItem::Verbatim(_) => todo!(),
//                         _ => todo!(),
//                     }
//                 }
//             }
//         }
//     }
// }

use std::fmt::Write;

pub fn modules_to_string(modules: &[JsModule], run_main: bool) -> String {
    let mut temp = String::new();
    for module in modules {
        write!(&mut temp, "{module}\n\n").unwrap();
    }
    if run_main {
        write!(&mut temp, "main();").unwrap();
    }
    temp
}

pub fn from_crate(crate_path: PathBuf, with_rust_types: bool, run_main: bool) -> String {
    let code = fs::read_to_string(crate_path.join("src").join("main.rs")).unwrap();
    let file = syn::parse_file(&code).unwrap();
    let items = file.items;

    // Crate path is eg "../for-testing/"
    let modules = process_items(items, Some(crate_path.clone()), with_rust_types, false);
    modules_to_string(&modules, run_main)
}

// Given every file *is* a module, and we concatenate all modules, including inline ones, into a single file, we should treat transpiling individual files *or* module blocks the same way
// Modules defined within a scope, eg a block, are not global and only accessible from that scope, but are treated the same way as other modules in that they are made global to the scope in which they are defined
// TODO really this should be `from_module_file` to be clear, since technically a file doesn't need to be a legitimate rust file, ie a module.
pub fn from_file(code: &str, with_rust_types: bool) -> Vec<JsModule> {
    let file = syn::parse_file(code).unwrap();
    let items = file.items;

    process_items(items, None, with_rust_types, false)
}

pub fn from_block(code: &str, with_rust_types: bool, _include_web: bool) -> Vec<JsStmt> {
    // TODO IMPORTANT why are we parsing the code a fn? this adds `return` in some cases which I keep forgetting and then waste time debugging!
    let item_fn = syn::parse_str::<Item>(&format!("fn temp() {code}")).unwrap();
    let modules = process_items(vec![item_fn], None, with_rust_types, true);
    // Blocks should only be 1 module and optionally include a second module for rust prelude
    // TODO why return the prelude module from process_items when it is not to be rendered?
    assert!(modules.len() == 1 || modules.len() == 2);
    let mut module = modules.into_iter().next().unwrap();
    // If we have inserted prelude statements, the len will be > 1. Ideally we would insert the prelude stmts inside `fn temp`. For now we are just assuming any added stmts are inserted before `fn temp`
    // assert!(module.stmts.len() == 1);
    // let temp_fn_wrapper = module.stmts.remove(0);
    let temp_fn_wrapper = module.stmts.remove(module.stmts.len() - 1);
    match temp_fn_wrapper {
        JsStmt::Function(js_fn) => js_fn.body_stmts,
        _ => todo!(),
    }
}

#[allow(clippy::vec_init_then_push)]
// pub fn from_block_old(code: &str, _with_rust_types: bool) -> Vec<JsStmt> {
//     // TODO should have a check to disallow use of `use` statement for `from_block` given we have no knowledge of the directory structure so can't lookup modules/crates in other files. NO because a block can still have inline modules. Should web prelude be allowed?

//     // let file = syn::parse_file(code).unwrap();
//     let expr_block = syn::parse_str::<ExprBlock>(code).unwrap();

//     // let mut names = Vec::new();
//     let mut modules = Vec::new();
//     modules.push(ModuleData {
//         name: "crate".to_string(),
//         // parent_name: None,
//         path: vec!["crate".to_string()],
//         // pub_definitions: Vec::new(),
//         // private_definitions: Vec::new(),
//         pub_submodules: Vec::new(),
//         private_submodules: Vec::new(),
//         pub_use_mappings: Vec::new(),
//         private_use_mappings: Vec::new(),
//         resolved_mappings: Vec::new(),
//         // fn_info: Vec::new(),
//         // item_definitons: Vec::new(),
//         // trait_definitons: Vec::new(),
//         // consts: Vec::new(),
//         various_definitions: VariousDefintions::default(),
//         items: Vec::new(),
//         scoped_various_definitions: Vec::new(),
//         scoped_syn_impl_items: Vec::new(),
//     });
//     let mut _get_names_module_path = ["crate".to_string()];

//     // let mut get_names_crate_path = crate_path.join("src/main.rs");
//     // let mut get_names_crate_path = crate_path.clone();
//     // extract_data(
//     //     &file.items,
//     //     None,
//     //     &mut get_names_module_path,
//     //     &mut names,
//     //     &mut modules,
//     // );

//     // let mut duplicates = Vec::new();
//     // for name in &names {
//     //     if names
//     //         .iter()
//     //         .filter(|(module_path, name2)| &name.1 == name2)
//     //         .collect::<Vec<_>>()
//     //         .len()
//     //         > 1
//     //     {
//     //         duplicates.push(Duplicate {
//     //             namespace: Vec::<String>::new(),
//     //             module_path: name.0.clone(),
//     //             name: name.1.clone(),
//     //             original_module_path: name.0.clone(),
//     //         });
//     //     }
//     // }
//     // update_dup_names(&mut duplicates);
//     // update_dup_names(&mut duplicates);
//     // update_dup_names(&mut duplicates);
//     // update_dup_names(&mut duplicates);
//     // update_dup_names(&mut duplicates);
//     // update_dup_names(&mut duplicates);

//     // for dup in duplicates.iter_mut() {
//     //     dup.namespace.push(dup.name.clone());
//     // }

//     // resolve_use_stmts(&mut modules);

//     let mut global_data = GlobalData::new(None);
//     global_data.modules = modules;

//     // populate_item_definitions(&mut global_data.modules);
//     // update_item_definitions(&mut global_data);
//     // populate_item_def_impl_blocks(&mut global_data);

//     global_data.transpiled_modules.push(JsModule {
//         public: true,
//         name: Ident::Str("crate"),
//         module_path: vec!["crate".to_string()],
//         stmts: Vec::new(),
//     });
//     // let stmts = js_stmts_from_syn_items(
//     //     file.items,
//     //     true,
//     //     &mut vec!["crate".to_string()],
//     //     &mut global_data,
//     //     &mut None,
//     // );

//     // It is better to parse this as an actual block expression and then just remove the braces/take the stmts within, because `handle_stmt()` will parse any items as being not module level, which means impls are added to the scope, which we don't have, so calling update_classes() tries to use module level impls which don't exist.
//     // let mut stmts = expr_block
//     //     .block
//     //     .stmts
//     //     .iter()
//     //     .map(|stmt| handle_stmt(stmt, &mut global_data, &vec!["crate".to_string()]).0)
//     //     .collect::<Vec<_>>();
//     // let (js_block, _rust_type) =
//     //     handle_expr_block(&expr_block, &mut global_data, &["crate".to_string()], false);
//     let stmts = match js_block {
//         JsExpr::Block(js_stmts) => js_stmts,
//         _ => todo!(),
//     };

//     // let stmts = if with_rust_types {
//     //     push_rust_types(&global_data, stmts)
//     // } else {
//     //     stmts
//     // };

//     let crate_module = global_data
//         .transpiled_modules
//         .iter_mut()
//         .find(|tm| tm.module_path == vec!["crate".to_string()])
//         .unwrap();
//     crate_module.stmts = stmts;

//     // update_classes(
//     //     &mut global_data.transpiled_modules,
//     //     &global_data.impl_items,
//     //     &global_data.default_trait_impls_class_mapping,
//     //     &global_data.default_trait_impls,
//     // );
//     // update_classes(
//     //     &mut global_data.transpiled_modules,
//     //     &global_data.impl_blocks,
//     //     &global_data.default_trait_impls_class_mapping,
//     //     &global_data.default_trait_impls,
//     // );

//     // and module name comments when there is more than 1 module
//     if global_data.transpiled_modules.len() > 1 {
//         for module in &mut global_data.transpiled_modules {
//             // dbg!(&module);
//             module.stmts.insert(
//                 0,
//                 JsStmt::Comment(if module.module_path == ["crate"] {
//                     "crate".to_string()
//                 } else {
//                     module
//                         .module_path
//                         .iter()
//                         .skip(1)
//                         .cloned()
//                         .collect::<Vec<_>>()
//                         .join("::")
//                 }),
//             );
//         }
//     }

//     global_data.transpiled_modules[0].stmts.clone()
// }

// TODO combine this with from_file
pub fn from_module(code: &str, _with_vec: bool) -> Vec<JsStmt> {
    let item_mod = syn::parse_str::<ItemMod>(code).unwrap();
    let items = item_mod.content.unwrap().1;
    let current_module = Vec::new();
    let mut global_data = GlobalData::new(None);
    js_stmts_from_syn_items(items, &current_module, &mut global_data)
}

pub fn from_fn(code: &str) -> Vec<JsStmt> {
    let item_fn = syn::parse_str::<ItemFn>(code).unwrap();

    let mut js_stmts = Vec::new();
    for stmt in &item_fn.block.stmts {
        let new_js_stmts = handle_stmt(stmt, &mut GlobalData::new(None), &Vec::new())
            .into_iter()
            .map(|(stmt, _type_)| stmt);
        js_stmts.extend(new_js_stmts);
    }
    js_stmts
}

// pub fn from_block(code: &str) -> Vec<JsStmt> {
//     let expr_block = syn::parse_str::<ExprBlock>(code).unwrap();

//     let mut js_stmts = Vec::new();
//     for stmt in &expr_block.block.stmts {
//         let js_stmt = handle_stmt(
//             stmt,
//             &mut GlobalData::new(true, None, Vec::new()),
//             &vec!["crate".to_string()],
//         );
//         js_stmts.push(js_stmt);
//     }
//     js_stmts
// }

fn _hardcoded_conversions(expr_path: &ExprPath, args: Vec<JsExpr>) -> Option<(JsExpr, RustType)> {
    let segments = expr_path
        .path
        .segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect::<Vec<_>>();

    if segments.last().unwrap() == "fetch2" {
        // TODO improve this code
        Some((
            JsExpr::FnCall(
                Box::new(JsExpr::Path(PathIdent::Single(Ident::Str("fetch")))),
                args,
            ),
            RustType::Todo,
        ))
    } else if segments.last().unwrap() == "stringify" {
        Some((
            JsExpr::FnCall(
                Box::new(JsExpr::Path(PathIdent::Path(vec![
                    Ident::Str("JSON"),
                    Ident::Str("stringify"),
                ]))),
                args,
            ),
            RustType::Todo,
        ))
    } else if segments.len() == 2 && segments[0] == "Json" && segments[1] == "parse" {
        Some((
            JsExpr::FnCall(
                Box::new(JsExpr::Path(PathIdent::PathTwo([
                    Ident::Str("JSON"),
                    Ident::Str("parse"),
                ]))),
                args,
            ),
            RustType::Todo,
        ))
    } else if segments.len() == 2 && segments[0] == "Date" && segments[1] == "from_iso_string" {
        Some((
            JsExpr::New(PathIdent::Single(Ident::Str("Date")), args),
            RustType::Todo,
        ))
    } else if segments.len() == 2
        && segments[0] == "Document"
        && segments[1] == "query_selector_body"
    {
        Some((
            JsExpr::FnCall(
                Box::new(JsExpr::Path(PathIdent::PathTwo([
                    Ident::Str("document"),
                    Ident::Str("querySelector"),
                ]))),
                vec![JsExpr::LitStr("body".to_string())],
            ),
            RustType::Todo,
        ))
    } else if segments.len() == 2
        && segments[0] == "Document"
        && segments[1] == "create_element_div"
    {
        Some((
            JsExpr::FnCall(
                Box::new(JsExpr::Path(PathIdent::PathTwo([
                    Ident::Str("document"),
                    Ident::Str("createElement"),
                ]))),
                vec![JsExpr::LitStr("div".to_string())],
            ),
            RustType::Todo,
        ))
    } else {
        None
    }
}

// NOTE I'm pretty sure get_path will currently convert something like `super::bar::baz::MyEnum::MyVariant` to `foo::bar::baz::MyEnum::MyVariant` ideally we would return the module path and item separately, but for now can just look up the item afterwards
// NOTE the segs returned might be namespaced with "__". This is something I plan to change and do the namespacing later
// So we have a path like foo::bar::baz()
// The algorithm for finding the full module path to the item is:
// Iif segs[0] is crate, super, or self, then jump to that module
// else
// Look to see if segs[0] is defined in any other the parent scopes
// else
// Look to see if segs[0] is defined at the module level
// else
// Look to see if segs[0] is a child module
// else
// Look to see if segs[0] is used

// TODO:
// handle mixed paths eg submodule -> use
// make sure `mod` being pub/private is taken into account - this would only be for use paths since mod is always public in the file it is called from (parent), so would happen in the `use` resolving step

// Take a path segs like foo::my_func(), and finds the absolute path to the item eg crate::bar::foo::my_func()
// Actually it should find the path relative to the seed path ie current_module, which is why it is useful to use recursively and for resolving use paths???
// What happens if the path is to a scoped item, or a variable (ie not an item definition)? We return the path of the item/var, which I believe in all cases must be a 0 length path/Vec<String>?
#[allow(clippy::too_many_arguments)]
fn _get_path_old(
    look_for_scoped_vars: bool,
    use_private_items: bool,
    // So we know whether allow segs to simply be somthing in an outer scope
    _module_level_items_only: bool,
    module: &ModuleData,
    mut segs: Vec<String>,
    global_data: &GlobalData,
    current_module: &[String],
    // Only used to determine if current module is
    original_module: &[String],
) -> Vec<String> {
    // TODO All parent modules are visible/public to their desecendants. When parents are accessed via `super`, it is easy the flag `use_private_items = true`. However when modules are accessed via `crate:: ...` I don't think there is any way to know whether the path leads a module which is a parent of (or is) the original module (ie so should be public), so we need to do this check. No - even if we access an item via `crate`, once we then visit a submodule, `use_private_items` get set to false, the problem is actually that sometimes we will want it to be true, when crate::submodule is actually a parent of the original module. So really we should just keep `is_parent_or_same_module` but a more efficient approach is to use `use_private_items` for crate, super, self, etc, then only calculate `is_parent_or_same_module` for submodule and pass as use_private_items
    let is_parent_or_same_module = if original_module.len() >= current_module.len() {
        current_module
            .iter()
            .enumerate()
            .all(|(i, current_module)| current_module == &original_module[i])
    } else {
        false
    };

    let item_defined_in_module =
        module.item_defined_in_module(use_private_items || is_parent_or_same_module, &segs[0]);

    // Whilst the immediate child modules are public to all modules (not their items, but the module itself), we might not actually be accessing it directly from the parent eg `foo::bar::baz()` and `bar` is not `pub`
    let path_starts_with_sub_module =
        module.path_starts_with_sub_module(use_private_items || is_parent_or_same_module, &segs[0]);

    // We are looking for the origin of some name so we want to compare that name with the item the use statement is importing
    // If we have a match then use want to add the use path to the path of the current module, eg:
    // current_module::crate::foo:item - we can overwrite the current module with "crate"
    // current_module::super::foo:item - we can pop the last seg of the current module
    // current_module::submodule::foo::item - add the submodule to the current module
    // current_module::another_used_item::item - we need to get the path of this subsequent used item in the same way described here, so we are recursing, then return to the original use path and carry on adding the rest of the segments
    let mut use_mappings = module.pub_use_mappings.iter();
    let matched_use_mapping = if use_private_items || is_parent_or_same_module {
        use_mappings
            .chain(module.private_use_mappings.iter())
            .find(|use_mapping| use_mapping.0 == segs[0])
    } else {
        use_mappings.find(|use_mapping| use_mapping.0 == segs[0])
    };
    // let path_starts_with_a_used_item_or_mod = module
    //     .resolved_mappings
    //     .iter()
    //     .find(|use_mapping| use_mapping.0 == segs[0]);

    let is_scoped = global_data.scopes.iter().rev().any(|scope| {
        // let is_func = scope.fns.iter().any(|func| func.ident == segs[0]);
        let is_func = false;
        // let is_item_def = scope
        //     .item_definitons
        //     .iter()
        //     .any(|item_def| item_def.ident == segs[0]);
        let is_item_def = false;

        // self could be and instance or module path ie `fn foo(&self) { self }` or `self::MyStruct`. I can't think of any situations where a module path can
        let is_var = scope.variables.iter().any(|var| var.name == segs[0])
            && segs.len() == 1
            && look_for_scoped_vars;

        // A scoped item must be the first element in the segs, ie in the original module so we need `current_module == original_module`
        (is_func || is_item_def || is_var) && current_module == original_module
    });

    // dbg!(&global_data.scopes);

    // TODO not sure why we need use_private_items here
    // if use_private_items && is_scoped {
    if is_scoped {
        // Variables and scoped items
        // Need to handle scoped vars and items first, otherwise when handling as module paths, we would always first have to check if the path is a scoped var/item

        // If we are returning a scoped var/item, no recursion should have occured so we should be in the same module
        assert!(current_module == original_module);
        segs
    } else if item_defined_in_module {
        // Path starts with an item defined in the module

        // Check whether it is not globally unique and so has been namespaced
        // TODO surely the global namespacing could and should happen just before writing the JS? It just get's in the way if it is already done at this point and we could just be using the module paths to differentiate
        if let Some(_dup) = global_data
            .duplicates
            .iter()
            .find(|dup| dup.name == segs[0] && dup.original_module_path == current_module)
        {
            // segs[0] = dup
            //     .namespace
            //     .iter()
            //     .map(camel)
            //     .collect::<Vec<_>>()
            //     .join("__");
        }

        // dbg!("item defined in module");

        segs
    } else if segs[0] == "super" {
        // TODO if a module level item name is shadowed by a item in a fn scope, then module level item needs to be namespaced
        segs.remove(0);

        let mut current_module = current_module.to_vec();
        current_module.pop();

        let module = global_data
            .modules
            .iter()
            .find(|module| module.path == current_module)
            .unwrap();

        // dbg!("in super");
        _get_path_old(
            false,
            true,
            true,
            module,
            segs,
            global_data,
            &current_module,
            original_module,
        )
    } else if segs[0] == "self" {
        // NOTE private items are still accessible from the module via self
        segs.remove(0);

        // I believe this works because the only effect of self is to look for the item only at the module level, rather than up through the fn scopes first, so get_path without the self and `in_same_module = false` achieves this, including handling any subsequent `super`s
        // TODO problem is that we are conflating `in_same_module` with pub/private
        // dbg!("in self");
        _get_path_old(
            false,
            true,
            true,
            module,
            segs,
            global_data,
            current_module,
            original_module,
        )
    } else if segs[0] == "crate" {
        let current_module = vec!["crate".to_string()];
        let module = global_data
            .modules
            .iter()
            .find(|module| module.path == current_module)
            .unwrap();

        // TODO descendants can access private items, so it depends whether the module trying to access the item is a descendant of the module it item is in, so need to keep track of the original call site?

        // dbg!("in crate");
        segs.remove(0);

        // NOTE all modules are desecendants of crate so all items in crate are visible/public
        _get_path_old(
            false,
            true,
            true,
            module,
            segs,
            global_data,
            &current_module,
            original_module,
        )
    } else if path_starts_with_sub_module {
        // Path starts with a submodule of the current module
        let mut submodule_path = current_module.to_vec();
        submodule_path.push(segs[0].clone());

        let submodule = global_data
            .modules
            .iter()
            .find(|submodule| submodule.path == submodule_path)
            .unwrap();

        // If we are going to be looking within the submodule that the current path starts with, we should remove it from the path since get_path() doesn't expect paths to a module to start with their name since this is not valid Rust, and self must be used instead.
        segs.remove(0);

        // dbg!("Path starts with a submodule of the current module");
        _get_path_old(
            false,
            false,
            true,
            submodule,
            segs,
            global_data,
            &submodule_path,
            original_module,
        )
    } else if let Some(use_mapping) = matched_use_mapping {
        let mut use_segs = use_mapping.1.clone();
        use_segs.push(use_mapping.0.clone());
        segs.remove(0);
        use_segs.extend(segs);
        let segs = _get_path_old(
            false,
            true,
            true,
            module,
            use_segs,
            global_data,
            current_module,
            original_module,
        );

        if let Some(_dup) = global_data
            .duplicates
            .iter()
            .find(|dup| dup.name == use_mapping.0 && dup.original_module_path == use_mapping.1)
        {
            // If the item has been namespaced, we need to replace it with the namespace
            // segs[0] = dup
            //     .namespace
            //     .iter()
            //     .map(camel)
            //     .collect::<Vec<_>>()
            //     .join("__");
            segs
        } else {
            // If the item has not been namespaced, we don't need to do anything
            segs
        }
        // TODO `this` is a JS ident, we want to be dealing with Rust idents at this point
    } else if segs.len() == 1 && segs[0] == "this" {
        segs
    } else if use_private_items {
        // Variables and scoped items

        // If none of the above conditions are met then assume the path refers to an item or variable in an enclosing scope within this module, not an item at the top level of a module, so simply return the path
        // dbg!("in private items");

        // Since this is the path to a scoped item or var, the path must be length 1
        // dbg!(&segs);
        assert!(segs.len() == 1);
        segs
    } else {
        dbg!(module);
        dbg!(current_module);
        dbg!(segs);
        panic!()
    }
}

// TODO this is needed because we want to combine the use_mapping Vec<String> with segs Vec<PathSegment>, but it might be better to just store use_mapping as Vec<PathSegment>.
#[derive(Debug, Clone)]
pub struct RustPathSegment {
    ident: String,
    turbofish: Vec<RustType>,
}

// return type for `handle_expr_path` because the path might not comprise a full expression/type, ie a tuple struct or enum variant that has args so requires being called
#[derive(Debug, Clone)]
enum PartialRustType {
    /// This is only used for tuple struct instantiation since normal struct instantiation are parsed to Expr::Struct and so can be directly evaluated to a struct instance, whereas a tuple struct instantiation is parsed as an ExprCall. Ok but Expr::Struct still has a `.path` `Path` field which we want to be able to parse/handle with the same handle_expr code, so now this can also be the path of a Expr::Struct
    ///
    /// So we are assuming that *all* cases where we have an Expr::Path and the final segment is a struct ident, it must be a tuple struct
    ///
    /// (type params, module path, name) module path is None for scoped structs
    StructIdent(Vec<RustTypeParam>, Vec<String>, Option<Vec<usize>>, String),
    /// This is only used for instantiation of enum variants with args which are parsed as an ExprCall, since normal enum variant instantiation are simply evaluated directly to an enum instance.
    /// Note we need to record type params because we might be parsing something like the `MyGenericEnum::<i32>::MyVariant` portion of `MyGenericEnum::<i32>::MyVariant("hi")` where the *enum definition* has had generics resolved
    ///
    /// (type params, module path, scope id, enum name, variant name) module path is None for scoped structs
    ///
    /// IMPORTANT NOTE this variant should only be used for tuple and struct variants, normal path variants should be a PartialRustType::RustType
    EnumVariantIdent(
        Vec<RustTypeParam>,
        Vec<String>,
        Option<Vec<usize>>,
        String,
        String,
    ),
    RustType(RustType),
}

/// For checking whether a struct item definition (possibly with resolved type params) matches the target type of a non-trait impl. Note this is not a simple equals since a Foo<i32> item matches a Foo<T> impl.
fn _struct_or_enum_types_match(
    target_type: &RustType,
    item_generics: &[RustTypeParam],
    item_module_path: &[String],
    item_scope_id: &Option<Vec<usize>>,
    item_name: &str,
) -> bool {
    // match impl_block.target {
    match target_type {
        RustType::StructOrEnum(
            target_type_params,
            target_module_path,
            target_scope_id,
            target_name,
        ) => {
            // Check that either:
            // 1. for each of the the path's type params, either the equivalent impl target's type param is generic or the path type param is concrete and matches the impl target's concrete type param
            // 2. the struct is not generic, so no type params
            assert!(item_generics.len() == target_type_params.len());

            let type_params_match = item_generics.iter().zip(target_type_params.iter()).all(
                |(struct_type_param, impl_target_type_params)| {
                    // Check both param lists are ordered the same way (ie the names match)
                    assert!(struct_type_param.name == impl_target_type_params.name);
                    match (&struct_type_param.type_, &impl_target_type_params.type_) {
                        (RustTypeParamValue::Unresolved, RustTypeParamValue::Unresolved) => true,
                        (RustTypeParamValue::Unresolved, RustTypeParamValue::RustType(_)) => false,
                        (RustTypeParamValue::RustType(_), RustTypeParamValue::Unresolved) => true,
                        (
                            RustTypeParamValue::RustType(struct_rust_type),
                            RustTypeParamValue::RustType(impl_target_rust_type),
                        ) => {
                            // TODO We might have nested generics eg a Foo<Bar<Baz<etc>>> concrete type, so need recursively do this checking until we find a type which has no generics
                            match (&**struct_rust_type, &**impl_target_rust_type) {
                                (RustType::NotAllowed, RustType::NotAllowed) => true,
                                (RustType::Unknown, RustType::Unknown) => true,
                                (RustType::Todo, RustType::Todo) => true,
                                (RustType::ParentItem, RustType::ParentItem) => true,
                                (RustType::Unit, RustType::Unit) => true,
                                (RustType::Never, RustType::Never) => true,
                                (RustType::ImplTrait(_), RustType::ImplTrait(_)) => true,
                                (RustType::TypeParam(_), RustType::TypeParam(_)) => true,
                                (RustType::I32, RustType::I32) => true,
                                (RustType::F32, RustType::F32) => true,
                                (RustType::Bool, RustType::Bool) => true,
                                (RustType::String, RustType::String) => true,
                                (RustType::Option(_), RustType::Option(_)) => true,
                                (RustType::Result(_), RustType::Result(_)) => true,
                                (
                                    RustType::StructOrEnum(_, _, _, _),
                                    RustType::StructOrEnum(_, _, _, _),
                                ) => true,
                                (RustType::Vec(_), RustType::Vec(_)) => true,
                                (RustType::Array(_), RustType::Array(_)) => true,
                                (RustType::Tuple(_), RustType::Tuple(_)) => true,
                                (RustType::UserType(_, _), RustType::UserType(_, _)) => true,
                                (RustType::MutRef(_), RustType::MutRef(_)) => true,
                                (RustType::Ref(_), RustType::Ref(_)) => true,
                                (RustType::Fn(_, _, _, _, _), RustType::Fn(_, _, _, _, _)) => true,
                                (_, _) => false,
                            }
                        }
                    }
                },
            );

            type_params_match
                && item_name == target_name
                && item_module_path == target_module_path
                && item_scope_id == target_scope_id
        }
        _ => false,
    }
}

pub fn generate_js_from_module(js: impl ToString) -> String {
    let stmts = from_module(js.to_string().as_str(), false);
    let mut temp = String::new();
    write!(&mut temp, "{}", stmts.fmt_join("\n")).unwrap();
    temp
}

pub fn format_js(js: impl ToString) -> String {
    let parse = biome_js_parser::parse_script(js.to_string().as_str(), JsParserOptions::default());
    let stmt = parse.syntax().children().nth(1).unwrap();
    let opts = JsFormatOptions::new(JsFileSource::default())
        .with_indent_width(IndentWidth::from(4))
        .with_indent_style(IndentStyle::Space);
    let formatted_js = biome_formatter::format_node(&stmt, JsFormatLanguage::new(opts)).unwrap();
    formatted_js.print().unwrap().as_code().to_string()
}
