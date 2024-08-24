use std::{path::PathBuf, rc::Rc};

use syn::{PathArguments, Type};
use tracing::debug;

use crate::{
    handle_syn::RustPathSegment2,
    js_ast::{Ident, JsFn, JsLocal, JsModule},
    make_item_definitions::{ItemRef, RustMod, StmtsRef},
    update_item_definitions::{
        FnDef, ItemDef, ItemDefRc, RustGeneric, RustImplItemItemNoJs, RustImplItemNoJs,
        RustTypeParam, RustTypeParamValue, StructEnumDef, TraitDef,
    },
    RUST_PRELUDE_MODULE_PATH,
};

// use super::handle_syn_item::JsImplItem;

#[derive(Debug, Clone)]
pub struct ScopedVar {
    pub name: String,
    pub mut_: bool,
    // TODO don't need this because it is part of the type and is record by the `type_` field
    // mut_ref: bool,
    // TODO
    pub type_: RustType2,
}
impl ScopedVar {
    #[allow(dead_code)]
    fn is_mut_ref(&self) -> bool {
        matches!(self.type_, RustType2::MutRef(_))
    }
}

#[derive(Debug, Clone)]
pub struct RustTypeParam2 {
    pub name: String,
    pub type_: RustTypeParamValue2,
}

#[derive(Debug, Clone)]
pub enum RustTypeParamValue2 {
    /// Can't be known at this point in analysis, eg the type is inferred somewhere else in the code
    Unresolved,
    RustType(Box<RustType2>),
}

#[derive(Debug, Clone)]
pub enum RustType2 {
    /// For cases/expressions we know cannot return a type, eg `break`
    NotAllowed,
    /// Can't be known at this point in analysis, eg the type is inferred somewhere else in the code
    Unknown,
    // /// Unitialized/null
    // Uninit,
    /// Needs implementing
    Todo,
    Self_,
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
    ImplTrait(Vec<RustTypeImplTrait2>),
    /// Why does RustTypeParam need to hold resolved values, surely when the param is resolved we just use that type directly? In some cases, eg a fn that returns T, if T is resolved we can just return the resolved type when the fn is called. Other times it might be that the type is resolved, but we need to know later down the line which param was resolved so we can resolve the param where it is used in other places??? Possible but need examples to justify it.
    TypeParam(RustTypeParam2),
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
    Option(RustTypeParam2),
    // Option(Box<RustType>),
    /// (generic)
    Result(RustTypeParam2),
    StructOrEnum(Vec<RustTypeParam2>, Rc<StructEnumDef>),
    // Struct(Vec<RustTypeParam>, Vec<String>, String),
    /// (type params, module path, name)  
    // Enum(Vec<RustTypeParam>, Vec<String>, String),
    // TODO Should we use the same type for both Arrays and Vecs, because they get transpiled to the same thing anyway? NO because we need to handle the types differently, ie arrays need `.copy()` adding when they are moved (although this won't be necessary if the previous value is not used after the move/copy, but this would be hard to determine so need to just always add copy).
    Vec(Box<RustType2>),
    Array(Box<RustType2>),
    Tuple(Vec<RustType2>),
    /// Even though Box::new() vanishes when transpiled, we need to keep track of which vars are Boxed because the dereferencing behaves differently
    Box(Box<RustType2>),
    /// ie `type FooInt = Foo<i32>;`
    /// (name, type)
    UserType(String, Box<RustType2>),
    /// (&mut T)
    MutRef(Box<RustType2>),
    /// (& T) useful to track & as well as &mut so we know what * is operating on?? NO I think it doesn't matter in practice, we can just check if we have a `&mut` expr and if not just ignore the *
    Ref(Box<RustType2>),
    /// (type params, return type)
    // Fn(Vec<RustTypeParam>, Box<RustType>),
    /// fn might be an associated fn in which case first arg will be Some() containing the (possibly resolved) generics of the impl target/self type. Possibly want to also record which type params are defined on the impl block, but see if we can get away without it initially given any impl block type params pretty much have to appear in the target/self type.
    /// (item type params, type params, module path, scope id, name)
    Fn(
        Option<Vec<RustTypeParam2>>,
        Vec<RustTypeParam2>,
        // RustTypeFnType,
        Rc<FnDef>,
    ),
    /// For things like Box::new where we want `Box::new(1)` -> `1`
    FnVanish,
    /// We need a separate type for closures because there is no definition with a path/ident to look up like RustType::Fn. Maybe another reason to store the type info directly and avoid using lookups so we don't need two separate variants.
    /// (input types, return type)
    Closure(Vec<RustType2>, Box<RustType2>),
}
impl RustType2 {
    pub fn is_js_primative(&self) -> bool {
        match self {
            RustType2::NotAllowed => todo!(),
            RustType2::Unknown => todo!(),
            RustType2::Todo => todo!(),
            RustType2::Unit => todo!(),
            RustType2::Never => todo!(),
            RustType2::ImplTrait(_) => todo!(),
            RustType2::TypeParam(_) => todo!(),
            RustType2::I32 | RustType2::F32 | RustType2::Bool | RustType2::String => true,
            RustType2::Option(inner) => {
                //
                match &inner.type_ {
                    RustTypeParamValue2::Unresolved => todo!(),
                    RustTypeParamValue2::RustType(resolved_type) => resolved_type.is_js_primative(),
                }
            }
            RustType2::Result(_) => todo!(),
            RustType2::StructOrEnum(_, _) => false,
            RustType2::Vec(_) => todo!(),
            RustType2::Array(_) => false,
            RustType2::Tuple(_) => todo!(),
            RustType2::UserType(_, _) => todo!(),
            RustType2::MutRef(_) => false,
            RustType2::Ref(_) => todo!(),
            RustType2::Fn(_, _, _) => false,
            RustType2::Closure(_, _) => todo!(),
            RustType2::FnVanish => todo!(),
            RustType2::Box(_) => todo!(),
            RustType2::Self_ => todo!(),
        }
    }
    pub fn is_mut_ref_of_js_primative(&self, _impl_targets: &[RustType2]) -> bool {
        match self {
            RustType2::NotAllowed => todo!(),
            RustType2::Unknown => todo!(),
            RustType2::Todo => todo!(),
            RustType2::Unit => false,
            RustType2::Never => todo!(),
            RustType2::ImplTrait(_) => todo!(),
            RustType2::TypeParam(_) => todo!(),
            RustType2::I32 => false,
            RustType2::F32 => false,
            RustType2::Bool => false,
            RustType2::String => false,
            RustType2::Option(_) => todo!(),
            RustType2::Result(_) => todo!(),
            RustType2::StructOrEnum(_, _) => false,
            RustType2::Vec(_) => todo!(),
            RustType2::Array(_) => todo!(),
            RustType2::Tuple(_) => todo!(),
            RustType2::UserType(_, _) => todo!(),
            RustType2::MutRef(inner) => inner.is_js_primative(),
            RustType2::Ref(_) => todo!(),
            RustType2::Fn(_, _, _) => todo!(),
            RustType2::Closure(_, _) => todo!(),
            RustType2::FnVanish => todo!(),
            RustType2::Box(_) => todo!(),
            RustType2::Self_ => todo!(),
        }
    }
}
impl PartialEq for RustType2 {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::ImplTrait(l0), Self::ImplTrait(r0)) => todo!(),
            (Self::TypeParam(l0), Self::TypeParam(r0)) => todo!(),
            (Self::Option(l0), Self::Option(r0)) => todo!(),
            (Self::Result(l0), Self::Result(r0)) => todo!(),
            (Self::StructOrEnum(l0, l1), Self::StructOrEnum(r0, r1)) => todo!(),
            (Self::Vec(l0), Self::Vec(r0)) => l0 == r0,
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
            (Self::Box(l0), Self::Box(r0)) => l0 == r0,
            (Self::UserType(l0, l1), Self::UserType(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::MutRef(l0), Self::MutRef(r0)) => l0 == r0,
            (Self::Ref(l0), Self::Ref(r0)) => l0 == r0,
            (Self::Fn(l0, l1, l2), Self::Fn(r0, r1, r2)) => todo!(),
            (Self::Closure(l0, l1), Self::Closure(r0, r1)) => l0 == r0 && l1 == r1,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum RustTypeImplTrait2 {
    SimpleTrait(Rc<TraitDef>),
    /// (return type)
    Fn(RustType2),
}

/// variable: Vec<ScopedVar>,
/// fns: Vec<FnInfo>,
/// generics: Vec<MyGeneric>,
/// structs_enums: Vec<StructOrEnumMethods>,
/// TODO remove `Default` to prevent accidentally creating `GlobalDataScope`s with a scope_id of []
#[derive(Debug, Clone)]
pub struct GlobalDataScope {
    // NOTE techincally we don't need this but it is useful to be able to reconcile with the static/preprocessed scopes to ensure we are talking about the same thing
    pub variables: Vec<ScopedVar>,
    pub items: Vec<usize>,
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
    pub _look_in_outer_scope: bool,
    pub use_mappings: Vec<(String, Vec<String>)>,
}

// TODO clean up these types since eg there is duplication of the fn ident
#[derive(Debug, Clone)]
pub struct RustImplItemJs {
    pub ident: String,
    pub item: RustImplItemItemJs,
    // return_type: RustType,
    // syn_object: ImplItem,
}

#[derive(Debug, Clone)]
pub enum RustImplItemItemJs {
    /// (static, fn info, js fn),
    Fn(bool, FnDef, JsFn),
    Const(JsLocal),
}

#[derive(Debug, Clone)]
pub struct JsImplBlock2 {
    // pub unique_id: String,
    pub _index: usize,
    pub _generics: Vec<RustGeneric>,
    pub trait_: Option<(Vec<String>, String, usize)>,
    // Note this can a generic param
    pub target: RustType2,
    /// Vec<(TODO whether the method actually gets used (for some eg `impl<T> Foo for T {}` blocks that apply to everything, it is hard to work out for which items the methods are actually used, since the criteria is whether the impl'd trait ie Foo is in scope/accessible at the point that the method is called, so the easiest approach is to just add everything and then track which methods actually get called), JS method/field)>
    pub items: Vec<(bool, RustImplItemJs)>,
}

impl JsImplBlock2 {
    pub fn js_name(&self) -> Ident {
        let trait_name = match &self.trait_ {
            Some((_module_path, name, _index)) => name,
            None => "no_trait",
        };
        fn rust_type_js_name(rust_type: &RustType2) -> String {
            match rust_type {
                RustType2::StructOrEnum(_, item_def) => {
                    // TODO get proper deduplicated js name
                    item_def.ident.clone()
                }
                RustType2::TypeParam(rust_type_param) => {
                    format!("for__{}", rust_type_param.name)
                }
                RustType2::Option(inner) => {
                    // The idea here is to differeniate between eg Option<T> and Option<i32>
                    let generic_name = match &inner.type_ {
                        RustTypeParamValue2::Unresolved => inner.name.clone(),
                        RustTypeParamValue2::RustType(resolved) => rust_type_js_name(resolved),
                    };
                    format!("Option_{}_", generic_name)
                }
                RustType2::I32 => {
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
pub struct GlobalData {
    pub _crate_path: Option<PathBuf>,
    // modules: Vec<ModuleData>,
    // TODO doesn't handle capturing scopes which needs rules to mimic how a closure decides to take &, &mut, or ownership
    // NOTE use separate Vecs for vars and fns because not all scopes (for vars) eg blocks are fns
    // NOTE don't want to pop fn after we finish parsing it because it will be called later in the same scope in which it was defined (but also might be called inside itself - recursively), so only want to pop it once it's parent scope completes, so may as well share scoping with vars
    // NOTE need to store vars and fns in the same Vec to ensure we know the precendence in cases like `fn foo() {}; fn bar() {}; let foo = bar;` NO - functions are hoisted so we always want to check if a var with that ident exists first *then* look for a fn, first in the scopes, then at the module level
    pub crates: Vec<RustMod>,
    // pub item_refs_to_render: Vec<ItemRef>,
    pub item_defs: Vec<ItemDefRc>,
    pub scopes: Vec<GlobalDataScope>,
    // TODO combine this with impl_items
    // struct_or_enum_methods: Vec<StructOrEnumMethods>,
    // scopes: Vec<Vec<ScopedVar>>,
    /// (the purpose originally was for self not Self... which is not needed, but Self is neccessary) the purpose of this is for storing the type of `Self` *not* `self`, eg if a impl fn body contains `let foo: Self = Self {};`, we will want to know what Self is so we know the types of `foo.some_field` etc
    ///
    /// We have a Vec in case there is an impl block nested inside an impl block?
    pub impl_block_target_type: Vec<RustType2>,
    /// Similar to impl_block_target_type but if for storing type params of the impl eg `impl<A, B> Foo<A, B> { ... }` so that when `A` and `B` appears in one of the impl's item definitions and we try and lookup the path `A` and `B` with `resolve_path()` we can also look here to find the type params.
    /// TODO Should be Vec of Vecs for same reason impl_block_target_type is a Vec
    _impl_block_type_params: Vec<RustTypeParam>,
    // TODO handle closures - which don't have explicitly specified return type, need to infer it from return value
    // scoped_fns: Vec<ItemFn>,
    pub rust_prelude_types: RustPreludeTypes,
    /// For temporary storage of JS methods prior to adding to JS classes
    /// TODO doesn't seem like we are actually populating this even though it has been used for a while?
    // impl_items_for_js: Vec<ImplItemTemp>,
    /// For looking up return types of methods etc
    // impl_items_for_types: Vec<(RustType, RustImplItem)>,
    // We keep the impl blocks at the crate level rather than in the relevant Module because different it is not possible to impl the same eg method name on the same struct, even using impl blocks in completely separate modules. Impl item idents must be unique for a given type across the entire crate. I believe this is also the case for scoped impls? This is because impl'd items are available on the item definition/instance they are targetting, not only in parent scopes, but also parent modules.
    // impl_blocks: Vec<ItemImpl>,
    pub impl_blocks: Vec<JsImplBlock2>,
    /// The purpose of having this here is so that all crate scoped impl blocks are immeditately available when parsing the syn to JS, eg if we come across a class (module level or scoped), we want to be able to add the methods which are implemented for it at that point, but these impls might not appear until later in the code, so instead we popualte scoped_impl_blocks in extract_data_populate_item_definitions to ensure it is available
    /// Given method names (impld for the same item) must be unqiue across the crate, for module level impls, we can just store all impl blocks in a big list, and add all their methods to all matching classes/items/enums/structs, whether the method is private/public is irrelevant since if it has been defined it must/should get used at some point.
    /// Scoped impls are a litte more complicated though, because in the same way we distinguish between different module level structs with the same name by taking into account their module path, for scoped structs we need to take into account the scope, ie a scoped `impl Foo { ... }` should only be applied to the first `Foo` that is found in parent scopes, else any module (of course taking into account the full module path used in `impl Foo { ... }`), because there might be another `Foo` in a higher scope with the same method impld, so we must not apply it there.
    /// We don't have to
    /// ((module path, scope id), rust impl block))
    // #[allow(clippy::type_complexity)]
    // _scoped_impl_blocks: Vec<((Vec<String>, Vec<usize>), JsImplBlock2)>,
    /// Testing: for the purpose of populating `item_definition.impl_items` see if we can store less info about impl blocks. We need the "signature" to be parsed so that we can easily determine whether the target is a type param or concrete type (or mixture - TODO), and also id's for the traits involved, ie the bounds on generics and the trait being impl.
    // pub impl_blocks_simpl: Vec<RustImplBlockSimple>,
    pub transpiled_modules: Vec<JsModule>,
    // /// For keeping track of whether we are parsing items at the module level or in a fn scope, so that we know whether we need to add the items to `.scopes` or not.
    // at_module_top_level: bool,
    // 1 based
    // scope_id: Vec<usize>,
    // 1 based
    // scope_count: Vec<usize>,
}
impl GlobalData {
    pub fn new(
        crate_path: Option<PathBuf>,
        crates: Vec<RustMod>,
        item_defs: Vec<ItemDefRc>,
    ) -> GlobalData {
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
            crates,
            item_defs,
            // init with an empty scope to ensure `scopes.last()` always returns something TODO improve this
            scopes: vec![GlobalDataScope {
                variables: Vec::new(),
                items: Vec::new(),
                _look_in_outer_scope: false,
                use_mappings: Vec::new(),
            }],
            impl_block_target_type: Vec::new(),
            _impl_block_type_params: Vec::new(),
            rust_prelude_types: RustPreludeTypes::default(),
            transpiled_modules: Vec::new(),
            impl_blocks: Vec::new(),
        }
    }

    pub fn get_trait(&self, index: usize) -> Rc<TraitDef> {
        let def = &self.item_defs[index];
        match def {
            ItemDefRc::Trait(trait_def) => trait_def.clone(),
            _ => todo!(),
        }
    }
    pub fn get_struct_enum(&self, index: usize) -> Rc<StructEnumDef> {
        let def = &self.item_defs[index];
        match def {
            ItemDefRc::StructEnum(item_def) => item_def.clone(),
            _ => todo!(),
        }
    }
    pub fn get_fn(&self, index: usize) -> Rc<FnDef> {
        let def = &self.item_defs[index];
        match def {
            ItemDefRc::Fn(fn_info) => fn_info.clone(),
            _ => todo!(),
        }
    }

    pub fn syn_type_to_rust_type_struct_or_enum(
        &self,
        current_module: &[String],
        // generics: &Vec<RustTypeParam>,
        syn_type: &Type,
    ) -> (Vec<RustTypeParam>, Vec<String>, String, ItemDefRc) {
        let type_path = match syn_type {
            Type::Path(type_path) => {
                type_path
                    .path
                    .segments
                    .iter()
                    .map(|seg| {
                        RustPathSegment2 {
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

        let (module_path, item_path, _is_scoped, item_def) = resolve_path(
            true,
            false,
            false,
            true,
            type_path,
            &self.crates,
            &self.item_defs,
            current_module,
            current_module,
            &self.scopes,
        );
        assert!(item_path.len() == 1);

        // let item_def = &self.item_defs[index.unwrap()];
        let struct_or_enum_def = match item_def.as_ref().unwrap() {
            ItemDefRc::StructEnum(def) => def,
            _ => todo!(),
        };
        (
            struct_or_enum_def
                .generics
                .iter()
                .map(|gen| RustTypeParam {
                    name: gen.clone(),
                    type_: RustTypeParamValue::Unresolved,
                })
                .collect::<Vec<_>>(),
            module_path.clone(),
            item_path.first().unwrap().ident.clone(),
            item_def.unwrap(),
        )
    }

    pub fn get_prelude_item_def(&self, name: &str) -> Rc<StructEnumDef> {
        let prelude_module = self.get_module(&[RUST_PRELUDE_MODULE_PATH.to_string()]);
        prelude_module
            .items
            .iter()
            .find_map(|item_ref| match item_ref {
                ItemRef::StructOrEnum(index) => {
                    let item = &self.item_defs[*index];
                    match item {
                        ItemDefRc::StructEnum(item_def) => {
                            (item_def.ident == name).then_some(item_def.clone())
                        }
                        _ => todo!(),
                    }
                }
                _ => None,
            })
            .unwrap()
    }

    // TODO IMPORTANT what if different crates have duplicate module paths??? We are assuming here that module_paths are unique across all crates.
    pub fn get_module(&self, module_path: &[String]) -> &RustMod {
        fn get_module_from_refs<'a, I>(item_refs: I, module_path: &[String]) -> Option<&'a RustMod>
        where
            I: IntoIterator<Item = &'a ItemRef>,
        {
            item_refs.into_iter().find_map(|item_ref| match item_ref {
                ItemRef::Mod(rust_mod) => {
                    if rust_mod.module_path == module_path {
                        Some(rust_mod)
                    } else {
                        let item_refs = rust_mod.items.iter();
                        get_module_from_refs(item_refs, module_path)
                    }
                }
                _ => None,
            })
        }
        // dbg!(&module_path);
        // dbg!(self
        //     .crates
        //     .iter()
        //     .map(|rust_mod| &rust_mod.module_path)
        //     .collect::<Vec<_>>());
        self.crates
            .iter()
            .find(|rust_mod| rust_mod.module_path == module_path)
            .unwrap_or_else(|| {
                let item_refs = self.crates.iter().flat_map(|rust_mod| &rust_mod.items);
                get_module_from_refs(item_refs, module_path).unwrap()
            })
    }

    pub fn is_web_prelude(&self, ident: &str) -> bool {
        // self.get_module(module_path)
        // TODO web prelude should be a crate, not a module
        todo!()
    }

    // This Doesn't/shouldn't look up methods as far as I can tell (methods are always handled directly in handle_expr_method_call) so rename
    // fn lookup_method_or_associated_fn(
    pub fn lookup_associated_fn(
        &self,
        item_generics: &[RustTypeParam2],
        _item_module_path: &[String],
        _item_scope_id: &Option<Vec<usize>>,
        sub_path: &RustPathSegment2,
        _item_path_seg: &str,
        item_def: &StructEnumDef,
        // item_def_index: usize,
        // ) -> Option<PartialRustType> {
        // TODO why return Option?
    ) -> Option<RustType2> {
        let impl_method = self.lookup_impl_item_item2(item_def, sub_path);

        let impl_method = if let Some(impl_method) = impl_method {
            match impl_method.item {
                RustImplItemItemNoJs::Fn(_static_, fn_info) => {
                    // If turbofish exists on fn path segment then use that for type params, otherwise use the unresolved params defined on the fn definition
                    let fn_generics = if !sub_path.turbofish.is_empty() {
                        sub_path
                            .turbofish
                            .iter()
                            .enumerate()
                            .map(|(i, g)| RustTypeParam2 {
                                name: fn_info.generics[i].clone(),
                                type_: RustTypeParamValue2::RustType(Box::new(g.clone())),
                            })
                            .collect::<Vec<_>>()
                    } else {
                        // NOTE for now we are assuming turbofish must exist for generic items, until we implement a solution for getting type params that are resolved later in the code
                        assert!(fn_info.generics.is_empty());
                        fn_info
                            .generics
                            .iter()
                            .map(|g| RustTypeParam2 {
                                name: g.clone(),
                                type_: RustTypeParamValue2::Unresolved,
                            })
                            .collect::<Vec<_>>()
                    };

                    Some(RustType2::Fn(
                        Some(item_generics.to_vec()),
                        fn_generics,
                        Rc::new(fn_info),
                    ))
                }
                RustImplItemItemNoJs::Const => todo!(),
            }
        } else {
            None
        };
        impl_method
    }

    // TODO what if the impl item is not defined on a struct/enum?
    pub fn lookup_impl_item_item2(
        &self,
        item_def: &StructEnumDef,
        sub_path: &RustPathSegment2,
        // TODO why return Option?
    ) -> Option<RustImplItemNoJs> {
        item_def.impl_block_ids.iter().find_map(|block_id| {
            let impl_block = match self.item_defs[*block_id].clone() {
                ItemDefRc::Impl(impl_block) => impl_block,
                _ => todo!(),
            };
            impl_block
                .rust_items
                .iter()
                .find(|rust_item| rust_item.ident == sub_path.ident)
                .cloned()
        })
    }
}

fn look_for_module_in_crates(
    crates: &[RustMod],
    item_defs: &[ItemDefRc],
    module_path: &[String],
) -> Option<RustMod> {
    for rust_mod in crates {
        if rust_mod.module_path == module_path {
            return Some(rust_mod.clone());
        }
        for item in &rust_mod.items {
            match item {
                ItemRef::Fn(index) => {
                    let item = &item_defs[*index];
                    let fn_info = match item {
                        ItemDefRc::Fn(fn_info) => fn_info,
                        _ => todo!(),
                    };

                    let fn_body_items = fn_info
                        .stmts
                        .clone()
                        .into_iter()
                        .filter_map(|stmt| {
                            match stmt {
                                StmtsRef::Item(item) => Some(item),
                                // TODO
                                // StmtsV1::Expr(_, _) => todo!(),
                                _ => None,
                            }
                        })
                        .collect::<Vec<_>>();

                    let found_rust_mod =
                        look_for_module_in_items(&fn_body_items, item_defs, module_path);
                    if found_rust_mod.is_some() {
                        return found_rust_mod;
                    }
                }
                ItemRef::Mod(rust_mod) => {
                    if rust_mod.module_path == module_path {
                        return Some(rust_mod.clone());
                    }
                    let found_rust_mod =
                        look_for_module_in_items(&rust_mod.items, item_defs, module_path);
                    if found_rust_mod.is_some() {
                        return found_rust_mod;
                    }
                }
                // TODO
                // ItemV1::Impl(_) => {}
                // ItemV1::Use(_) => {}
                _ => {}
            }
        }
    }
    None
}
fn look_for_module_in_items(
    items: &[ItemRef],
    item_defs: &[ItemDefRc],
    module_path: &[String],
) -> Option<RustMod> {
    for item in items {
        match item {
            ItemRef::Fn(index) => {
                let item = &item_defs[*index];
                let fn_info = match item {
                    ItemDefRc::Fn(fn_info) => fn_info,
                    _ => todo!(),
                };

                let fn_body_items = fn_info
                    .stmts
                    .clone()
                    .into_iter()
                    .filter_map(|stmt| {
                        match stmt {
                            StmtsRef::Item(item) => Some(item),
                            // TODO
                            // StmtsV1::Expr(_, _) => todo!(),
                            _ => None,
                        }
                    })
                    .collect::<Vec<_>>();

                let found_rust_mod =
                    look_for_module_in_items(&fn_body_items, item_defs, module_path);
                if found_rust_mod.is_some() {
                    return found_rust_mod;
                }
            }
            ItemRef::Mod(rust_mod) => {
                if rust_mod.module_path == module_path {
                    return Some(rust_mod.clone());
                }
                let found_rust_mod =
                    look_for_module_in_items(&rust_mod.items, item_defs, module_path);
                if found_rust_mod.is_some() {
                    return found_rust_mod;
                }
            }
            // TODO
            // ItemV1::Impl(_) => {}
            // ItemV1::Use(_) => {}
            _ => {}
        }
    }
    None
}

// TODO ideally test tracing output in get_path test cases to ensure the expect code path is being taken??
// TODO need to make sure this looks up traits as well as other items
/// -> (current module (during recursion)/item module path (upon final return), found item path, found item scope id)
///
/// TODO maybe should return Option<Vec<String>> for the module path to make it consistent with the rest of the codebase, but just returning a bool is cleaner
///
/// TODO given eg `use MyEnum::{Variant1, Variant2};` we need to not only look for match `ItemDefintion`s but also matching enum variants
pub fn resolve_path(
    // For determining whether we have just been given a single length path and thus should also look for scoped items/vars.
    // NOTE we can't just rely on `current_mod == orig_mod` or `current_mod.len() == 1` because these will be incorrectly `true` in case like the second recursion of `self::foo` which is just `foo`.
    // TODO find better name
    orig_len_1: bool,
    _look_for_scoped_vars: bool,
    // TODO can we combine this with `look_for_scoped_vars`?
    _look_for_scoped_items: bool,
    use_private_items: bool,
    mut segs: Vec<RustPathSegment2>,
    // TODO replace GlobalData with `.modules` and `.scopes` to making setting up test cases easier
    // module_items: &[ItemRef],
    crates: &[RustMod],
    // TODO replace GlobalData with `.modules` and `.scopes` to making setting up test cases easier
    // global_data: &GlobalData,
    items_defs: &[ItemDefRc],
    current_mod: &[String],
    // Only used to determine if current module is the original module
    orig_mod: &[String],
    // TODO scopes would ideally be set to None when resovle_path is called recursively since that means the path length is > 1 which is not possible for scoped vars and items, however it is possible for a scoped use_mapping to have path length > 1.
    // scopes: &Option<Vec<GlobalDataScope>>,
    scopes: &Vec<GlobalDataScope>,
    // (module path, item path, is scoped, index)
) -> (Vec<String>, Vec<RustPathSegment2>, bool, Option<ItemDefRc>) {
    debug!(segs = ?segs, "get_path_without_namespacing");

    // TODO I don't think we need to pass in the module `ModuleData` if we are already passing the `current_module` module path we can just use that to look it up each time, which might be less efficient since we shouldn't need to lookup the module if we haven't changed modules (though I think we are pretty much always changing modules except for use statements?), but we definitely don't want to pass in both. Maybe only pass in `module: &ModuleData` and not `current_module`
    // assert!(current_module == &module.path);

    // dbg!(current_mod);
    // dbg!(crates
    //     .iter()
    //     .map(|rust_mod| &rust_mod.module_path)
    //     .collect::<Vec<_>>());
    let module = look_for_module_in_crates(crates, items_defs, current_mod).unwrap();

    let is_parent_or_orig_module = if orig_mod.len() >= current_mod.len() {
        current_mod
            .iter()
            .enumerate()
            .all(|(i, current_module)| current_module == &orig_mod[i])
    } else {
        false
    };

    let single_element_path = segs.len() == 1;

    let use_private = use_private_items || is_parent_or_orig_module;

    let item_defined_in_module =
        module.item_defined_in_module2(items_defs, use_private, &segs[0].ident);

    let path_starts_with_sub_module = module.path_starts_with_sub_module(
        use_private_items || is_parent_or_orig_module,
        &segs[0].ident,
    );

    // TODO only look through transparent scopes
    // We look through scopes, simultaneously looking for a matching var, use_mapping, or item
    #[derive(Debug, Clone)]
    enum ScopedThing {
        UseMapping((String, Vec<String>)),
        Var(ScopedVar),
        Item(ItemDefRc),
    }

    #[allow(clippy::manual_map)]
    let scoped_thing = scopes.iter().rev().find_map(|s| {
        // TODO it is wasteful to pre-find these elements but the expressions are too long to embed directly in if statements. Could make fns or closure but that is extra verbose. Probably want methods on GlobalDataScope which take closures which we can create here.
        let scoped_var = s
            .variables
            .iter()
            .find(|v| single_element_path && orig_len_1 && v.name == segs[0].ident);
        let scoped_item = s.items.iter().find(|index| {
            let item_def = &items_defs[**index];
            orig_len_1 && item_def.ident() == segs[0].ident
        });
        if let Some(use_mapping) = s.use_mappings.iter().find(|u| u.0 == segs[0].ident) {
            Some(ScopedThing::UseMapping(use_mapping.clone()))
        } else if let Some(scoped_var) = scoped_var {
            Some(ScopedThing::Var(scoped_var.clone()))
        } else if let Some(scoped_item) = scoped_item {
            let item_def = items_defs[*scoped_item].clone();
            Some(ScopedThing::Item(item_def))
        } else {
            None
        }
    });

    let matched_use_mapping =
        if let Some(ScopedThing::UseMapping(scoped_use_mapping)) = &scoped_thing {
            Some(scoped_use_mapping.clone())
        } else {
            module.items.iter().find_map(|item| match item {
                ItemRef::Use(rust_use) => rust_use.use_mapping.iter().find_map(|use_mapping| {
                    (use_mapping.0 == segs[0].ident && (use_private || rust_use.pub_))
                        .then_some(use_mapping.clone())
                }),
                _ => None,
            })
        };

    // TODO can module shadow external crate names? In which case we need to look for modules first? I think we do this implicitly based on the order of the if statements below?
    // TODO actually look up external crates in Cargo.toml
    // NOTE for external crates which are not the rust prelude and so use-stmts are needed, we can simply check if the beginning of the current segs is equal to the name of an external crate and then below we change the current_module to simply be vec![<name of that crate>].
    // NOTE we need to handle the *rust* prelude uniquely since although we treat is as a separate crate, there of course isn't any use stmts, so we just have to look for rust prelude items as the final thing after everything else has been checked.
    let external_crate_names = ["web_prelude"];
    let path_is_external_crate = external_crate_names.iter().any(|cn| cn == &segs[0].ident);

    // TODO IMPORTANT we have two `is_scoped` vars here because we are using `get_path` in different contexts. `is_scoped_static` is for getting the path from static data, before syn -> JS parsing, and `is_scoped` is for use during the syn -> JS parsing. This needs thinking about, reconciling and simplifying. Should just stop using get_path for vars.

    // TODO IMPORTANT
    // We cannot have a scoped item/fn definition with the same ident as a module, but we can have a scoped *var* with the same ident as a module/item/fn. I think Rust just chooses which one to use based on the context eg foo::bar must be an item/module, foo.bar() must be an instance/var, etc. To follow this approach would mean we need more context for this fn.
    // eg this is aloud:
    // use tracing;
    // let tracing = "ohno";
    // As far as I am aware the only common context in which a path might have length=1 is a use statement, which doesn't use this fn to resolve the path (though it probably should given we have to follow the same crate/self/super logic?) so for now just assume that if we match a scoped var name and the length is 1, then return the scoped var, even though technically it could be a module/item/fn eg this is valid: *NO* what about a simple `Foo {}` which is a path with length 1 where we could also have `let Foo = 5;`, which would make it impossible to decide which to return, eg module level struct (Some("crate"), ["Foo"]) vs scoped var (None, ["Foo"]).**
    // struct foo {}
    // fn main() {
    //     let foo = 5;
    // }
    // For scoped modules/items/fns there is currently no difference anyway since we currently just return Vec<RustPathSegment> regardless.
    // The main problem is the above example. However, the below is not valid which I believe demonstrates that it the ident must be unambigious if it can be used as eg a fn argument, and so it is indeed the context of where the path is being used eg `bar(foo);`, `let bar = foo;`, `foo {}`, etc which determines which thing to use. I think it will be non trivial to pass handle handle the different contexts to this fn.
    // struct foo;
    // fn main() {
    //     let foo = 5;
    // }
    // ** The distinction is between where the site of the path expects a definition (eg fn input type) and instances (eg assign to a variable). A slight complication is that both of the below are valid, just not simultaneously, but it still means that a path passed the the rhs of an assignment could be and instance *or* a definition, so we need to look for other if one doesn't exist, **but *only* if we have `struct foo;` and not `struct foo {}` because for the latter we *are* allowed both idents in scope, so need to ensure we choose the scoped var, in this case. This is as apposed to say the type of a fn input where we can always be sure to not look for scoped vars, only any items/fns.
    // let foo = 5;
    // let which = foo;
    // ...
    // struct foo;
    // let which = foo;
    // So I think maybe the trick is to pass an argument to this fn to say wether we should be considering vars and follow these rules:
    // 1. If including vars (eg simple 1-len path assignment) then look for a var first, else look for a struct, this way we will catch assigning `struct foo;` because a `foo` var can't exist in this case, and for `struct Foo {}` we will correctly pick the var first.
    // 2. If not inlcluding vars we simply don't have to look for vars.
    // Don't need both can just always do step 1?

    // self could be and instance or module path ie `fn foo(&self) { self }` or `self::MyStruct`. I can't think of any situations where a module path can

    if let Some(ScopedThing::Var(_scoped_var)) = scoped_thing {
        // Variables and scoped items
        // Need to handle scoped vars and items first, otherwise when handling as module paths, we would always first have to check if the path is a scoped var/item
        // If we are returning a scoped var/item, no recursion should have occured so we should be in the same module
        assert!(current_mod == orig_mod);
        // (current_mod.clone(), segs, is_scoped_static)
        (current_mod.to_vec(), segs, true, None)
    } else if let Some(ScopedThing::Item(index)) = scoped_thing {
        assert!(current_mod == orig_mod);
        (current_mod.to_vec(), segs, true, Some(index))
    } else if item_defined_in_module.is_some() {
        (current_mod.to_vec(), segs, false, item_defined_in_module)
    } else if segs[0].ident == "super" {
        // TODO if a module level item name is shadowed by an item in a fn scope, then module level item needs to be namespaced
        segs.remove(0);

        let mut current_module = current_mod.to_vec();
        current_module.pop();

        resolve_path(
            false,
            false,
            false,
            true,
            segs,
            crates,
            items_defs,
            &current_module,
            orig_mod,
            scopes,
        )
    } else if segs[0].ident == "self" {
        // NOTE private items are still accessible from the module via self
        segs.remove(0);

        resolve_path(
            false,
            false,
            false,
            true,
            segs,
            crates,
            items_defs,
            current_mod,
            orig_mod,
            scopes,
        )
    } else if segs[0].ident == "crate" {
        let current_module = vec!["crate".to_string()];

        segs.remove(0);

        resolve_path(
            false,
            false,
            false,
            true,
            segs,
            crates,
            items_defs,
            &current_module,
            orig_mod,
            scopes,
        )
    } else if path_starts_with_sub_module {
        // Path starts with a submodule of the current module
        let mut submod_path = current_mod.to_vec();
        submod_path.push(segs[0].ident.to_string());

        segs.remove(0);

        resolve_path(
            false,
            false,
            false,
            false,
            segs,
            crates,
            items_defs,
            &submod_path,
            orig_mod,
            scopes,
        )
    } else if let Some(use_mapping) = matched_use_mapping {
        // Use mappings the resolved path for each item/module "imported" into the module with a use statement. eg a module containing
        // `use super::super::some_module::another_module;` will have a use mapping recorded of eg ("another_module", ["crate", "top_module", "some_module"])
        // So say we have a path like `another_module::MyStruct;`, then we will match this use mapping and the below code combines the path from the mapping and the current "segs" to make `"crate", "top_module", "some_module", "another_module", "MyStruct";`
        // What if we have a path like `another_module::yet_another_module::MyStruct;`??

        // TODO I think we need to set the current_module to use_mapping.1, remove this from segs (ie just not add it), and then we can just start from that module in the next get_path iteration?? NO That causes tests to fail

        let mut use_segs = use_mapping.1.clone();
        use_segs.push(use_mapping.0.clone());
        // TODO IMPORTANT seems like we are not correctly populating turbofish here
        let mut use_segs = use_segs
            .into_iter()
            .map(|s| RustPathSegment2 {
                ident: s,
                turbofish: Vec::new(),
            })
            .collect::<Vec<_>>();
        segs.remove(0);
        use_segs.extend(segs);

        // TODO IMPORTANT for a `use` statement for a third party crate, we need to set the `current_module` accordingly. I think it is fine to just use ["name_of_crate", "module_in_crate", etc].

        // TODO do we not need to update the current module if the use path/mapping has taken us to a new module?? write some tests NO because the use mapping just provides an absolute path to the module/item that is used which is of course valid from the current module. The important part is that the get_path iteration after this `matched_use_mapping` iteration

        // let new_mod = if use_segs[0].ident == "crate" {
        //     // current_mod
        //     use_mapping.1.clone()
        // } else if external_crate_names
        //     .iter()
        //     .any(|cn| cn == &use_segs[0].ident)
        // {
        //     use_mapping.1.clone()
        // } else {
        //     panic!()
        // };

        resolve_path(
            false,
            false,
            false,
            true,
            use_segs,
            crates,
            items_defs,
            // &new_mod,
            current_mod,
            // &use_mapping.1.clone(),
            orig_mod,
            scopes,
        )
    // } else if segs.len() == 1 && segs[0] == "this" {
    //     segs
    } else if path_is_external_crate {
        // } else if false {
        // TODO need to update current_mod

        // Handle equivalently to segs[0] == "crate"
        let crate_name = segs.remove(0);
        let current_module = [crate_name.ident].to_vec();

        resolve_path(
            false,
            false,
            false,
            true,
            segs,
            crates,
            items_defs,
            &current_module,
            orig_mod,
            scopes,
        )
    } else {
        // Handle third party crates
        // TODO lookup available crates in Cargo.toml
        // if segs[0..3].iter().map(|s| &s.ident).collect::<Vec<_>>()
        //     == ["ravascript", "prelude", "web"]
        // {
        //     return (
        //         vec!["prelude_special_case".to_string()],
        //         segs[3..].to_vec(),
        //         false,
        //     );
        // }

        // If we can't find the ident anywhere, the only remaining possibility is that we have a prelude type
        assert_eq!(current_mod, orig_mod);
        assert!(segs.len() == 1 || segs.len() == 2);
        let rust_prelude_crates = crates
            .iter()
            .find(|rust_mod| rust_mod.module_path == [RUST_PRELUDE_MODULE_PATH])
            .unwrap();

        let seg = &segs[0];
        let new_ident = if seg.ident == "Some" || seg.ident == "None" {
            "Option"
        } else {
            &seg.ident
        };
        let item_def = rust_prelude_crates
            .items
            .iter()
            .find_map(|item_ref| match item_ref {
                ItemRef::StructOrEnum(index) => {
                    let item = &items_defs[*index];
                    (item.ident() == new_ident).then_some(item.clone())
                }
                ItemRef::Fn(index) => {
                    let item = &items_defs[*index];
                    (item.ident() == new_ident).then_some(item.clone())
                }
                ItemRef::Const(index) => {
                    let item = &items_defs[*index];
                    (item.ident() == new_ident).then_some(item.clone())
                }
                ItemRef::Trait(index) => {
                    let item = &items_defs[*index];
                    (item.ident() == new_ident).then_some(item.clone())
                }
                _ => None,
            });
        if let Some(item_index) = item_def {
            (
                vec![RUST_PRELUDE_MODULE_PATH.to_string()],
                segs,
                false,
                Some(item_index),
            )
        } else {
            dbg!("resolve_path couldn't find path");
            // dbg!(module);
            dbg!(current_mod);
            // dbg!(current_scope_id);
            dbg!(segs);
            panic!()
        }
    }
}

// Mutable/state data
#[derive(Default, Clone, Debug)]
pub struct RustPreludeTypes {
    pub vec: bool,
    hash_map: bool,
    // TODO need to check which fns return an Option, and set `option` for those
    pub option: bool,
    pub some: bool,
    // TODO Is this just null?
    pub none: bool,
    result: bool,
    ok: bool,
    err: bool,
    assert_eq: bool,
    assert_ne: bool,
    dbg: bool,
    // println: bool,
    // print: bool,
    pub number_prototype_extensions: bool,
    pub string_prototype_extensions: bool,
    boolean_prototype_extensions: bool,
    pub integer: bool,
    pub float: bool,
    pub string: bool,
    pub bool: bool,
    pub equals: bool,

    // New approach
    // Basic `RustInteger`
    pub rust_integer: bool,
    pub rust_string: bool,
    pub rust_array_copy: bool,
    pub option_is_some_and: bool,
    pub option_unwrap: bool,
    pub result_is_err: bool,
    pub result_unwrap: bool,
}
