use syn::{ItemEnum, ItemImpl, ItemStruct, ItemTrait, Member};
use tracing::debug;

use crate::{
    handle_syn::{GlobalData, RustType2, RustTypeImplTrait2, RustTypeParam2, RustTypeParamValue2},
    js_ast::Ident,
    make_item_definitions::FnInfoSyn,
    tree_structure::{ExprRef, StmtsRef},
};

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone)]
pub enum StructFieldInfo {
    UnitStruct,
    TupleStruct(Vec<RustType>),
    /// (name, type)
    RegularStruct(Vec<(String, RustType)>),
}

#[derive(Debug, Clone)]
pub struct StructDefinitionInfo {
    pub fields: StructFieldInfo,
    pub syn_object: ItemStruct,
}

#[derive(Debug, Clone)]
pub enum EnumVariantInputsInfo {
    Named {
        ident: String,
        input_type: RustType,
    },
    /// (input type)
    Unnamed(RustType),
}

#[derive(Debug, Clone)]
pub struct EnumVariantInfo {
    pub ident: String,
    pub inputs: Vec<EnumVariantInputsInfo>,
}

#[derive(Debug, Clone)]
pub struct EnumDefinitionInfo {
    pub members: Vec<EnumVariantInfo>,
    pub syn_object: ItemEnum,
}

#[derive(Debug, Clone)]
pub enum StructOrEnumDefitionInfo {
    Struct(StructDefinitionInfo),
    Enum(EnumDefinitionInfo),
}

/// Similar to StructOrEnum which gets used in RustType, but is for storing info about the actual item definition, rather than instances of, so eg we don't need to be able to store resolved generics. Minor differences but making distinct type helps with reasoning about the different use cases.
/// Just structs and enums or should we include functions?
#[derive(Debug, Clone)]
pub struct ItemDefinition {
    pub js_name: Ident,
    pub ident: String,
    // NOTE we don't need to store the module path because module level `ItemDefinition`s are stored within modules so we will already know the module path
    // module_path: Option<Vec<String>>,
    pub is_copy: bool,
    pub is_pub: bool,
    // /// Fields and enum variants. Methods etc are stored in impl blocks?
    // members: Vec<StructFieldInfo>,
    // members: Vec<ImplItem>,
    // TODO do we need to know eg bounds for each generic?
    pub generics: Vec<String>,
    // syn_object: StructOrEnumSynObject,
    pub struct_or_enum_info: StructOrEnumDefitionInfo,
    // impl_blocks: Vec<ItemDefintionImpls>,
    /// Should be used for matching only inherent impls to the target type, which is always be a struct or enum (or type alias of a struct or enum)? but trait impls also currently are added
    ///
    /// Currently used for:
    /// -   looking up impl items
    /// -   add static fields (pointing to trait class) and methods to JsClass (update_classes_stmts())
    /// -   adding stmts like `Number.prototype.foo = bar.prototype.foo` (handle_syn_item())
    ///
    /// These are uuids/references to all the impl blocks whose target match this struct/enum
    ///
    /// (unique impl id)
    // TODO use reference instead of id?
    pub impl_block_ids: Vec<usize>,
}
impl ItemDefinition {
    pub fn get_type(&self, field_member: &Member, global_data: &GlobalData) -> RustType2 {
        match &self.struct_or_enum_info {
            StructOrEnumDefitionInfo::Struct(struct_def_info) => match &struct_def_info.fields {
                StructFieldInfo::UnitStruct => todo!(),
                StructFieldInfo::TupleStruct(_) => todo!(),
                StructFieldInfo::RegularStruct(fields2) => fields2
                    .iter()
                    .find_map(|(field_name, field_type)| {
                        let field_member_name = match field_member {
                            Member::Named(ident) => ident.to_string(),
                            Member::Unnamed(_) => todo!(),
                        };
                        (field_name == &field_member_name)
                            .then_some(field_type.clone().into_rust_type2(global_data))
                    })
                    .unwrap()
                    .clone(),
            },
            StructOrEnumDefitionInfo::Enum(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RustTraitDefinition {
    pub js_name: Ident,
    pub name: String,
    pub is_pub: bool,
    // impl_items:
    pub syn: ItemTrait,
    pub default_impls: Vec<FnInfo>,
}

// Do we want to also store the trait bounds of each type param? This way if we have an unresolved type param that calls some function, we will know what trait to look up to find it. In some cases this might also remove the need for looking forward to resolve type params, if all we need to do with the type param/value/intance/type is call a method on it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustTypeParam {
    pub name: String,
    pub type_: RustTypeParamValue,
}
impl RustTypeParam {
    pub fn into_rust_type_param2(self, global_data: &GlobalData) -> RustTypeParam2 {
        RustTypeParam2 {
            name: self.name,
            type_: match self.type_ {
                RustTypeParamValue::Unresolved => RustTypeParamValue2::Unresolved,
                RustTypeParamValue::RustType(type_) => {
                    RustTypeParamValue2::RustType(Box::new(type_.into_rust_type2(global_data)))
                }
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustTypeParamValue {
    /// Can't be known at this point in analysis, eg the type is inferred somewhere else in the code
    Unresolved,
    RustType(Box<RustType>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustTypeImplTrait {
    SimpleTrait(usize),
    /// (return type)
    Fn(RustType),
}

#[derive(Debug, Clone)]
pub struct RustImplItemNoJs {
    pub ident: String,
    pub item: RustImplItemItemNoJs,
    // return_type: RustType,
    // syn_object: ImplItem,
}

#[derive(Debug, Clone)]
pub enum RustImplItemItemNoJs {
    /// (static, fn info),
    Fn(bool, FnInfo),
    Const,
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
// NOTE we include specialised types like RustType::I32 for performance reasons to avoid needing to do a comparison like `module_path == [...]` everytime we want to check for an integer or string which is often given they are the most common types
// NOTE we need ParentItem because we can use `Self` in expressions like `let foo = Self { foo: 5 };` etc, but we should avoid using it in definitions, only for syn parsing/expression code
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustType {
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
    // ParentItem,
    /// ()
    Unit,
    /// !
    Never,
    /// Fns might return impl FooTrait, and that will also be the type of eg any var that the result is assigned to. It's fine not knowing the exact type because you can only call the trait's methods on it, but need to be able to look up the trait's methods to know what type they return.
    ///
    /// Vec<(module path, scope id, RustTypeImplTrait (ie simple FooTrait or a fn trait: Fn(i32) -> i32))>
    ImplTrait(Vec<RustTypeImplTrait>),
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
    /// (type params, module path, name, index)
    StructOrEnum(Vec<RustTypeParam>, Vec<String>, String, usize),
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
        // TODO arguably it would be better to just store the path and item name all in one, and when looking up the item/fn we are able to determine at that point whether the final one or two elements of the path are a item or associated fn or whatever
        RustTypeFnType,
        usize,
    ),
    /// For things like Box::new where we want `Box::new(1)` -> `1`
    FnVanish,
    /// We need a separate type for closures because there is no definition with a path/ident to look up like RustType::Fn. Maybe another reason to store the type info directly and avoid using lookups so we don't need two separate variants.
    /// (input types, return type)
    Closure(Vec<RustType>, Box<RustType>),
}
impl RustType {
    pub fn into_rust_type2(self, global_data: &GlobalData) -> RustType2 {
        match self {
            RustType::NotAllowed => RustType2::NotAllowed,
            RustType::Unknown => RustType2::Unknown,
            RustType::Todo => RustType2::Todo,
            RustType::Unit => RustType2::Unit,
            RustType::Never => RustType2::Never,
            RustType::ImplTrait(traits) => RustType2::ImplTrait(
                traits
                    .into_iter()
                    .map(|trait_| match trait_ {
                        RustTypeImplTrait::SimpleTrait(index) => {
                            let trait_def = global_data.get_trait(index);
                            RustTypeImplTrait2::SimpleTrait(trait_def)
                        }
                        RustTypeImplTrait::Fn(_) => todo!(),
                    })
                    .collect(),
            ),
            RustType::TypeParam(rust_type_param) => {
                RustType2::TypeParam(rust_type_param.into_rust_type_param2(global_data))
            }
            RustType::I32 => RustType2::I32,
            RustType::F32 => RustType2::F32,
            RustType::Bool => RustType2::Bool,
            RustType::String => RustType2::String,
            RustType::Option(rust_type_param) => {
                RustType2::Option(rust_type_param.into_rust_type_param2(global_data))
            }
            RustType::Result(rust_type_param) => {
                RustType2::Result(rust_type_param.into_rust_type_param2(global_data))
            }
            RustType::StructOrEnum(type_params, module_path, name, index) => {
                let item_def = global_data.get_struct_enum(index);
                RustType2::StructOrEnum(
                    type_params
                        .into_iter()
                        .map(|tp| tp.into_rust_type_param2(global_data))
                        .collect(),
                    item_def,
                )
            }
            RustType::Vec(inner) => RustType2::Vec(Box::new(inner.into_rust_type2(global_data))),
            RustType::Array(inner) => {
                RustType2::Array(Box::new(inner.into_rust_type2(global_data)))
            }
            RustType::Tuple(inner) => RustType2::Tuple(
                inner
                    .into_iter()
                    .map(|type_| type_.into_rust_type2(global_data))
                    .collect(),
            ),
            RustType::Box(inner) => RustType2::Box(Box::new(inner.into_rust_type2(global_data))),
            RustType::UserType(name, inner) => {
                RustType2::UserType(name, Box::new(inner.into_rust_type2(global_data)))
            }
            RustType::MutRef(inner) => {
                RustType2::MutRef(Box::new(inner.into_rust_type2(global_data)))
            }
            RustType::Ref(inner) => RustType2::Ref(Box::new(inner.into_rust_type2(global_data))),
            RustType::Fn(item_type_params, type_params, module_path, name, index) => {
                let fn_info = global_data.get_fn(index);
                RustType2::Fn(
                    item_type_params.map(|tps| {
                        tps.into_iter()
                            .map(|tp| tp.into_rust_type_param2(global_data))
                            .collect()
                    }),
                    type_params
                        .into_iter()
                        .map(|tp| tp.into_rust_type_param2(global_data))
                        .collect(),
                    Box::new(fn_info),
                )
            }
            RustType::FnVanish => RustType2::FnVanish,
            RustType::Closure(inputs, return_) => RustType2::Closure(
                inputs
                    .into_iter()
                    .map(|type_| type_.into_rust_type2(global_data))
                    .collect(),
                Box::new(return_.into_rust_type2(global_data)),
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustTypeFnType {
    /// (fn name)
    Standalone(String),
    /// (item name, fn name)
    AssociatedFn(String, String),
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub js_name: Ident,
    pub name: String,
    pub is_pub: bool,
    pub type_: RustType,
    pub expr: ExprRef,
}

/// Not just for methods, can also be an enum variant with no inputs
#[derive(Debug, Clone)]
pub struct FnInfo {
    // TODO No point storing all the info like inputs and return types separately, as these need to be stored on RustType::Fn anyway for eg closures where we won't be storing a fn info?? Keep both for now and revisit later. Note fns idents can just appear in the code and be called whereas a closure will be a var which already has a type.
    pub js_name: Ident,
    pub ident: String,
    pub is_pub: bool,
    /// Does this include receiver/self types? NO in handle_item_fn we are filtering out any self type. Could just store it as RustType::Self, but seems pointless if we don't actually need it for anything. NO again, updated to include self inputs because we need them.
    /// TODO probably don't actually need `is_self`
    /// (is_self, is_mut, name, type)
    pub inputs_types: Vec<(bool, bool, String, RustType)>,
    pub generics: Vec<String>,
    // NO! for methods we want to store the actual fn type. fns can be assigned to vars, and we want to be able to pass the Path part of the fn, and *then* call it and determine the return type
    pub return_type: RustType,
    // /// type of fn eg Fn(i32) -> ()
    // rust_type: RustType,
    // TODO optionally add enum for Field, AssociatedFn, Method, etc
    pub syn: FnInfoSyn,

    pub stmts: Vec<StmtsRef>,
}

impl FnInfo {
    pub fn attempt_to_resolve_type_params_using_arg_types(
        &self,
        args: &[RustType2],
    ) -> Vec<RustTypeParam2> {
        self.generics
            .iter()
            .map(|g| {
                let matched_arg_rust_type = self.inputs_types.iter().enumerate().find_map(
                    |(i, (_is_self, _is_mut, _name, input_type))| {
                        match input_type {
                            RustType::TypeParam(type_param) if g == &type_param.name => {
                                Some(args[i].clone())
                            }
                            // TODO what about types that *contain* a type param eg `foo: Option<T>`
                            _ => None,
                        }
                    },
                );

                let rust_type_param_value =
                    if let Some(matched_arg_rust_type) = matched_arg_rust_type {
                        RustTypeParamValue2::RustType(Box::new(matched_arg_rust_type))
                    } else {
                        RustTypeParamValue2::Unresolved
                    };

                RustTypeParam2 {
                    name: g.clone(),
                    type_: rust_type_param_value,
                }
            })
            .collect::<Vec<_>>()
    }
}

#[derive(Debug, Clone)]
pub struct RustGeneric {
    pub ident: String,
    // (module path, trait name)
    // pub trait_bounds: Vec<(Vec<String>, Option<Vec<usize>>, String)>,
    pub trait_bounds: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct RustImplBlockSimple {
    // pub unique_id: String,
    // TODO Should this include generics that are defined on the target type, or just new generics introduced for the impl Trait or used in the methods/items? For now just assume it is everything.
    pub generics: Vec<RustGeneric>,
    /// NOTE we include module and name for generating a unique name for the impl block. TODO probably not best approach, what about identical signature?
    /// (module, name, index)
    pub trait_: Option<(Vec<String>, String, usize)>,
    // Note this can a generic param
    pub target: RustType,
    pub rust_items: Vec<RustImplItemNoJs>,
    // items: Vec<ImplItem>,
    pub syn: ItemImpl,
}
