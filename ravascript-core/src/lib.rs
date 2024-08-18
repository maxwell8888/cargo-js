#![allow(clippy::too_many_arguments)]
// #![allow(unused_variables)]
// #![allow(dead_code)]

use biome_formatter::{IndentStyle, IndentWidth};
use biome_js_formatter::{context::JsFormatOptions, JsFormatLanguage};
use biome_js_parser::JsParserOptions;
use biome_js_syntax::JsFileSource;

use handle_syn::{js_stmts_from_syn_items, GlobalData, RustImplItemItemJs, RustType2};
use js_ast::{Ident, JsClass, JsExpr, JsFn, JsModule, PathIdent};
use make_item_definitions::{extract_modules2, ItemActual, ItemRef, RustMod, StmtsRef};
use std::{fmt::Debug, fs, path::PathBuf};
use syn::{ExprPath, Item, ItemTrait};
use tracing::debug_span;

mod handle_syn;
mod js_ast;
pub mod prelude;
pub mod rust_prelude;

mod duplicate_namespacing;
use duplicate_namespacing::namespace_duplicates;

mod make_item_definitions;

mod update_item_definitions;
use update_item_definitions::{
    update_item_definitions2, ItemDefinition, ItemV2, RustImplBlockSimple, RustType, RustTypeParam,
    RustTypeParamValue,
};

pub use js_ast::JsStmt;

const PRELUDE_MODULE_PATH: &str = "prelude_special_case";

// TODO need to handle expressions which return `()`. Probably use `undefined` for `()` since that is what eg console.log();, var x = 5;, etc returns;
// TODO preserve new lines so generated js is more readable
// TODO consider how to get RA/cargo check to analyze rust inputs in `testing/`

// TODO I think we want to add *usertype* impls and *impl trait for usertype* (in both cases where the usertype is not generic) as methods to the type's class, but for all other cases, eg *impl trait for T* impls we want to keep the trait impl as an object of fns/class with methods and then still add equivalent methods to the type classes but rather than include the fn body, just point to these fns/methods in the impl block object. This avoids eg potentially duplicating fn bodys on *all* type classes if eg we have `impl<T> MyTrait for T` {}. Though the cost of duplicating the fn bodys should be irrelevant with compression... so the best reason to do this is probably to avoid noise when reading the transpiled JS, and to make the code feel more idiomatic/familiar to the Rust dev.
// What about somthing like `impl MyTrait for Foo<Bar> { ... }` and `impl MyTrait for Foo<Baz> { ... }`. The methods should live on the Foo class, but the bodies/impls are different depending on the generic eg `let foo = Foo(Bar()).some_trait_method();` vs `let foo = Foo(Baz()).some_trait_method();`
// eg `let foo = Foo(Bar()).some_trait_method();` vs `let foo = Foo(Baz()).some_trait_method();`
// Or even just `impl Foo<Bar> { ... }` and `impl Foo<Baz> { ... }`.
// So need to monomorphize generic types that

fn get_traits_implemented_for_item(
    item_impls: &[(usize, RustImplBlockSimple)],
    item_module_path: &[String],
    item_name: &str,
    // TODO change to Vec<usize>
) -> Vec<(Vec<String>, String, usize)> {
    // Does class implement the trait bounds of the impl block

    // TODO this code is duplicated from elsewhere
    // Each time we find new traits we need to again look for matching traits, and repeat this until we don't find any new traits
    let mut found_traits_count = 0;
    // (trait module path (None for scoped), trait name)
    let mut found_traits: Vec<(Vec<String>, String, usize)> = Vec::new();
    loop {
        found_traits.clear();

        for (_index, item_impl) in item_impls {
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
                    let struct_impls_all_bounds =
                        type_param_bounds.iter().all(|type_param_bound| {
                            found_traits
                                .iter()
                                .map(|(_module_path, _name, index)| *index)
                                .collect::<Vec<_>>()
                                .contains(type_param_bound)
                        });
                    if struct_impls_all_bounds {
                        found_traits.push(rust_trait);
                    }
                }
                RustType::StructOrEnum(_, _, _, _) | RustType::I32 => {
                    if let Some(impl_trait) = &item_impl.trait_ {
                        let types_match = match &item_impl.target {
                            RustType::StructOrEnum(_type_params, module_path, name, _index) => {
                                module_path == item_module_path && name == item_name
                            }
                            RustType::I32 => {
                                item_module_path == [PRELUDE_MODULE_PATH] && item_name == "i32"
                            }
                            RustType::Option(_) => {
                                item_module_path == [PRELUDE_MODULE_PATH] && item_name == "Option"
                            }
                            _ => {
                                dbg!(&item_module_path);
                                dbg!(&item_name);
                                todo!()
                            }
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

/// Populates `item_def.impl_blocks: Vec<String>` with ids of impl blocks
fn populate_item_def_impl_blocks(
    item_refs: &[ItemRef],
    item_defs: &mut [ItemV2],
    // impl_blocks: &[(usize, RustImplBlockSimple)],
) {
    // IMPORTANT TODO all this needs improving, especially to ensure we are only trying to match scoped impls that can actually reach the item. Need unit tests.

    let span = debug_span!("update_classes");
    let _guard = span.enter();

    fn extract_impl_blocks(
        item_refs: &[ItemRef],
        item_defs: &[ItemV2],
        impl_blocks: &mut Vec<(usize, RustImplBlockSimple)>,
    ) {
        for item_ref in item_refs {
            match item_ref {
                ItemRef::Impl(index) => {
                    let impl_def = match item_defs[*index].clone() {
                        ItemV2::Impl(impl_def) => impl_def,
                        _ => todo!(),
                    };
                    impl_blocks.push((*index, impl_def))
                }
                ItemRef::Mod(rust_mod) => {
                    extract_impl_blocks(&rust_mod.items, item_defs, impl_blocks);
                }
                ItemRef::Fn(index) => match item_defs[*index].clone() {
                    ItemV2::Fn(fn_info) => {
                        let item_refs = fn_info
                            .clone()
                            .stmts
                            .into_iter()
                            .filter_map(|stmt_ref| match stmt_ref {
                                StmtsRef::Item(item_ref) => Some(item_ref),
                                _ => None,
                            })
                            .collect::<Vec<_>>();
                        extract_impl_blocks(&item_refs, item_defs, impl_blocks)
                    }
                    _ => todo!(),
                },
                _ => {}
            }
        }
    }
    let mut impl_blocks = Vec::new();
    extract_impl_blocks(item_refs, item_defs, &mut impl_blocks);
    // dbg!(impl_blocks
    //     .iter()
    //     .map(|(index, block)| (index, &block.syn.self_ty))
    //     .collect::<Vec<_>>());

    // Recursively update all structs/enums
    fn update_all_structs_enums(
        item_refs: &[ItemRef],
        item_defs: &mut [ItemV2],
        current_module: Vec<String>,
        impl_blocks: &[(usize, RustImplBlockSimple)],
    ) {
        for item_ref in item_refs {
            match item_ref {
                ItemRef::StructOrEnum(index) => {
                    let actual = item_defs.get_mut(*index).unwrap();
                    let item_def = match actual {
                        ItemV2::StructOrEnum(def) => def,
                        _ => todo!(),
                    };
                    update_item_def_block_ids(
                        *index,
                        item_def,
                        // &item_def_scope_id,
                        &current_module,
                        impl_blocks,
                    )
                }
                ItemRef::Mod(rust_mod) => {
                    update_all_structs_enums(
                        &rust_mod.items,
                        item_defs,
                        rust_mod.module_path.clone(),
                        impl_blocks,
                    );
                }
                ItemRef::Fn(index) => match item_defs[*index].clone() {
                    ItemV2::Fn(fn_info) => {
                        let item_refs = fn_info
                            .clone()
                            .stmts
                            .into_iter()
                            .filter_map(|stmt_ref| match stmt_ref {
                                StmtsRef::Item(item_ref) => Some(item_ref),
                                _ => None,
                            })
                            .collect::<Vec<_>>();
                        update_all_structs_enums(
                            &item_refs,
                            item_defs,
                            current_module.clone(),
                            impl_blocks,
                        )
                    }
                    _ => todo!(),
                },
                _ => {}
            }
        }
    }

    // TODO assuming first recursion should be finding only modules so can just past empty module name to start
    update_all_structs_enums(item_refs, item_defs, vec![], &impl_blocks);

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
    item_index: usize,
    item_def: &mut ItemDefinition,
    // item_def_scope_id: &Option<Vec<usize>>,
    module_path: &[String],
    // global_data: &GlobalData,
    impl_blocks: &[(usize, RustImplBlockSimple)],
) {
    let traits_impld_for_class =
        get_traits_implemented_for_item(impl_blocks, module_path, &item_def.ident);
    // for impl_block in impl_blocks.iter().chain(scoped_impl_blocks.clone()) {
    for (index, impl_block) in impl_blocks {
        // NOTE we differentiate between concrete and type param targets because for (non-generic TODO) concrete types we only have to match on item name/id, whereas for type params we have to check if the item implements all the type bounds
        match &impl_block.target {
            // Concrete type target
            RustType::StructOrEnum(
                _,
                _impl_target_module_path,
                _impl_target_name,
                impl_tartget_index,
            ) => {
                // The purpose of storing this info on the item_def is so that after the syn -> JS parsing parsing has happened and we have a parsed impl block and items, we can use this info to lookup this parsed impl block and copy it's methods/fields to the class.
                if item_index == *impl_tartget_index {
                    item_def.impl_block_ids.push(*index);
                }
                // if &item_def.ident == impl_target_name && module_path == impl_target_module_path {
                //     // item_def
                //     //     .impl_blocks
                //     //     .push(ItemDefintionImpls::ConcreteImpl(impl_block.items.clone()));

                //     item_def.impl_block_ids.push(*index);
                // }
            }
            RustType::I32 => {
                if item_def.ident == "i32" && module_path == [PRELUDE_MODULE_PATH] {
                    item_def.impl_block_ids.push(*index);
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
                let struct_impls_all_bounds = type_param_bounds.iter().all(|type_param_bound| {
                    traits_impld_for_class
                        .iter()
                        .map(|(_module_path, _name, index)| *index)
                        .collect::<Vec<_>>()
                        .contains(type_param_bound)
                });

                // TODO we might be adding this impl to items that meet the trait bound, but methods from the impl'd trait are not acutally called. There doesn't seem to be any easy, direct way to check this, the best approach is possibly for each method stored on the `ItemDefinition` to have a flag which get's set to true when the method is called/looked up, and otherwise remains false and is not used/output when the item/class is written to JS.

                if struct_impls_all_bounds {
                    item_def.impl_block_ids.push(*index);
                }
            }
            // IMPORTANT NOTE I did have a todo! here but it would without fail cause rust-analyzer to crash when I moved my cursor there, and take down vscode with it
            _ => {}
        }
    }
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

// Impl handling:
// make_item_definitions pushes all syn Impls to `module.syn_impl_items`.
// update_item_definitions then loops over these and creates RustImplBlockSimple and (indirectly/eventually) pushes to `GlobalData.impl_blocks_simpl`.
// It is worth noting that `GlobalData.impl_blocks_simpl` only ever gets used by `GlobalData.lookup_impl_item_item2()` and in `handle_impl_item()` as mentioned below.
// populate_item_def_impl_blocks simply populates ItemDefinition.impl_block_ids
// `handle_item_impl()` gets the matching RustImplBlockSimple(s) (gets a Vec???) from global_data.impl_blocks_simpl for *all* syn Impls, and (partly) uses this to create a `JsImplBlock2` (namely the `RustImplItemJs`s), and pushes to GlobalData.impl_blocks. We then create a `JsClass` for the impl (if it is not an inherent impl) and the static fields and methods from the `JsImplBlock2`. We then push stmts like `Number.prototype.foo = bar.prototype.foo` for each prelude type which matches/impls the impl.
// update_classes2: for each JsClass that is not for an impl block, for each impl block it impls (ie has id in it's impl_block_ids) we get the `JsImplBlock2` from global_data.impl_blocks and copy it's static fields and methods to the JsClass

// Plan: remove update_classes2
// During syn->JS handling, for structs and enums, already know their impls. We look for inherent impls and trait impls that target a concrete type and parse them directly in the struct/enum handler, so that we can directly add the methods etc to the JsClass, and then there is no need to do anything for that impl in handle_item_impl(), so we can just ignore inherent impls and trait impls that target a concrete type in handle_item_impl(). Note that for traits with a default that is not overwritten in the impl block, we will need to take the below approach.
// For trait impls that don't target a concrete type, we simply add a field/method/prototype field pointing to the trait impl class/object that we know will exist. In the case that the type param only ends up matching a single concrete type, we could just add it directly to the type class like above, but this is not a priority optimisation, so just ignore this for now.
// What about generics in either above case?:
// If the impl doesn't specify any concrete types from the target type, we can just treat it the same as a non-generic target type.
// In the case where there is more than one impl, and at least one of them specifies a concrete type for one of the type params, we need to create to classes eg `class Foo_i32 {}` and `class Foo_f32 {}` and add methods etc to them accordingly.
// UPDATE still need to do this. Made an attempt but gave up because I ran into scope_id issues.
pub fn process_items(
    items: Vec<Item>,
    crate_path: Option<PathBuf>,
    // TODO I don't think there is much point in supporting generation without "Rust types" so remove this flag
    with_rust_types: bool,
    // We use this to know whether we should insert eg
    is_block: bool,
) -> Vec<JsModule> {
    let mut item_defs = Vec::new();

    // TODO need borrowed items for inline `mod {}` and owned items for `mod foo` where we read from a file

    // gets names of module level items, creates `ModuleData` for each sub module, and adds `use` data to module's `.use_mapping`
    let crate_item_refs = extract_modules2(
        true,
        &items,
        &crate_path,
        &mut vec!["crate".to_string()],
        &mut item_defs,
    );

    // TODO needs to be able to distinguish between `web_prelude` which is being using as a third party crate and something that has been defined in the code, ie I think any time we find a `web_prelude` we need to check if there is any user defined item or var with the same name in scope
    // TODO make this robust against user modules named "web_prelude" - we need to resolve the path/use_mapping to the crate. The web_prelude crate (or rustscript::web_prelude or whatever we end up using) is unique because crate names can't be shadowed (NOT TRUE - only on crates.io. It is possible for users to have a local crate named web_prelude or be using an alternative it crates.io?? But surely we can know this from the Cargo.toml because the dep table with have a path, or an alternative repository will be specified??).
    let include_web = crate_item_refs.iter().any(|item_ref| match item_ref {
        ItemRef::Use(rust_use) => rust_use
            .use_mapping
            .iter()
            .any(|use_mapping| use_mapping.1 == ["web_prelude"]),
        _ => false,
    });

    let mut crate_item_refs = vec![ItemRef::Mod(RustMod {
        pub_: true,
        items: crate_item_refs,
        module_path: vec!["crate".to_string()],
    })];

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
    // So better to just rely on Cargo.toml dependencies, and not support web_prelude and other crates in blocks/formats without a Cargo.toml?

    if include_web {
        let web_prelude_crate_path = "../web-prelude";
        // We include the web prelude at compile time so that it can be used for eg from_block or from_file which operate on simple strings of Rust code and no Cargo project
        // let code = fs::read_to_string(web_prelude_entry_point_path).unwrap();
        let code = include_str!("../../web-prelude/src/lib.rs");
        let file = syn::parse_file(code).unwrap();
        let prelude_items = file.items;

        let web_prelude_items = extract_modules2(
            true,
            &prelude_items,
            &Some(web_prelude_crate_path.into()),
            &mut vec!["web_prelude".to_string()],
            &mut item_defs,
        );

        crate_item_refs.insert(
            0,
            ItemRef::Mod(RustMod {
                pub_: true,
                items: web_prelude_items,
                module_path: vec!["web_prelude".to_string()],
            }),
        );
    };

    // Rust prelude
    let code = include_str!("rust_prelude/option.rs");
    let file = syn::parse_file(code).unwrap();
    let prelude_items = file.items;

    let mut rust_prelude_items = extract_modules2(
        true,
        &prelude_items,
        // TODO for now use None since we are using a single file but probably want to eventually expand to some kind of fake "lib"
        &None,
        &mut vec![PRELUDE_MODULE_PATH.to_string()],
        &mut item_defs,
    );
    // Need to manually add the Fn traits because we can't redefine them to allow them be read in with all the prelude items.
    item_defs.push(ItemActual::Trait(
        make_item_definitions::RustTraitDefinition {
            name: "FnOnce".to_string(),
            is_pub: true,
            syn: syn::parse_str::<ItemTrait>("trait FnOnce {}").unwrap(),
            default_impls: Vec::new(),
        },
    ));
    rust_prelude_items.push(ItemRef::Trait(item_defs.len() - 1));
    item_defs.push(ItemActual::Trait(
        make_item_definitions::RustTraitDefinition {
            name: "Copy".to_string(),
            is_pub: true,
            syn: syn::parse_str::<ItemTrait>("trait Copy {}").unwrap(),
            default_impls: Vec::new(),
        },
    ));

    rust_prelude_items.push(ItemRef::Trait(item_defs.len() - 1));
    crate_item_refs.insert(
        0,
        ItemRef::Mod(RustMod {
            pub_: true,
            items: rust_prelude_items,
            module_path: vec![PRELUDE_MODULE_PATH.to_string()],
        }),
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
    // let mut actual_modules = make_item_definitions(modules);

    // find duplicates
    let duplicates = namespace_duplicates(&crate_item_refs, &item_defs);

    // populates `global_data.impl_blocks_simpl` and defs that use types like a structs fields in it's ItemDef, fn arguments, etc
    // TODO re updating item defs here because we need to be able to lookup other types used in item defs which might appear later: if we update extract_data to gather the location of items, rather than just their idents, we could use that data and do it all in populate_item_definitions rather than needing to do some here... although that does mean we would need to start tracking the scope in `extract_data` which we currently don't need to so that seems suboptimal
    // let (mut new_modules, impl_blocks) = update_item_definitions(actual_modules, duplicates);

    let mut item_defs = update_item_definitions2(
        &crate_item_refs,
        item_defs,
        &["crate".to_string()],
        false,
        duplicates,
    );

    // global_data_crate_path is use when reading module files eg global_data_crate_path = "../my_crate/" which is used to prepend "src/some_module/submodule.rs"

    // Match `RustImplBlockSimpl`s to item definitions. It is necessary to do it at this stage so that we can look up method info when parsing syn -> JS. We also use this in update_classes2 to know which parsed JS impls to lookup to add their methods/fields to the class. What??? there doesn't seem to be any JS parsing here?
    // iterates through `global_data.impl_blocks_simpl`'s `RustImplBlockSimple`s to populate `item_def.impl_blocks` with `ItemDefintionImpls`s
    // TODO need to also look through the scoped `RustImplBlockSimple` and populate either scoped *or* module level `item_def.impl_blocks`s with `ItemDefintionImpls`s
    // Populates `item_def.impl_blocks: Vec<String>` with ids of impl blocks
    populate_item_def_impl_blocks(&crate_item_refs, &mut item_defs);

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

    let mut global_data = GlobalData::new(crate_path, crate_item_refs, item_defs);
    // global_data.modules = new_modules;
    // global_data.impl_blocks_simpl = impl_blocks;

    let item_refs = global_data.item_refs.clone();
    // This is intentionally only extracting the crate and prelude modules, for now
    let top_level_modules = item_refs
        .iter()
        .filter_map(|item_ref| match item_ref {
            ItemRef::Mod(rust_mod) => (rust_mod.module_path != ["web_prelude"]
                && rust_mod.module_path != [PRELUDE_MODULE_PATH])
            .then_some(rust_mod),
            _ => None,
        })
        .collect::<Vec<_>>();
    // global_data.item_refs_to_render = modules;

    // We need/want to keep modules in tree for syn parsing to make it easy to lookup module submodules etc. However, we also want flattened modules to make it easy to iterate over them and render distinct modules

    // let mut transpiled_modules = Vec::new();

    for rust_mod in top_level_modules {
        global_data.scopes.clear();

        let (mut stmts, mut submodules) =
            js_stmts_from_syn_items(&rust_mod.items, &rust_mod.module_path, &mut global_data);

        if with_rust_types && rust_mod.module_path == ["crate"] {
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

        global_data.transpiled_modules.insert(
            0,
            JsModule {
                public: true,
                name: Ident::String(rust_mod.module_path.last().unwrap().clone()),
                module_path: rust_mod.module_path.clone(),
                stmts,
            },
        );
        global_data.transpiled_modules.append(&mut submodules);
    }

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
    // let global_data_copy = global_data.clone();
    // update_classes2(&mut global_data.transpiled_modules, &global_data_copy);

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

    // TODO can this not just be done automatically in JsModule.js_string()? Would require a flag arg so we can avoid the comments for blocks etc, but this is preferrable to polluting this fn with something so trivial.
    // add module name comments when there is more than 1 module

    let mut transpiled_modules = global_data.transpiled_modules;
    if transpiled_modules.len() > 1 {
        for module in transpiled_modules
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

    transpiled_modules
}

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

// TODO should have a check to disallow use of `use` statement for `from_block` given we have no knowledge of the directory structure so can't lookup modules/crates in other files. NO because a block can still have inline modules, just need to error if the module path leads nowhere just like we do for a transpiling a crate. Should web prelude be allowed?

// make sure `mod` being pub/private is taken into account - this would only be for use paths since mod is always public in the file it is called from (parent), so would happen in the `use` resolving step

// TODO All parent modules are visible/public to their desecendants. When parents are accessed via `super`, it is easy the flag `use_private_items = true`. However when modules are accessed via `crate:: ...` I don't think there is any way to know whether the path leads a module which is a parent of (or is) the original module (ie so should be public), so we need to do this check. No - even if we access an item via `crate`, once we then visit a submodule, `use_private_items` get set to false, the problem is actually that sometimes we will want it to be true, when crate::submodule is actually a parent of the original module. So really we should just keep `is_parent_or_same_module` but a more efficient approach is to use `use_private_items` for crate, super, self, etc, then only calculate `is_parent_or_same_module` for submodule and pass as use_private_items
// Whilst the immediate child modules are public to all modules (not their items, but the module itself), we might not actually be accessing it directly from the parent eg `foo::bar::baz()` and `bar` is not `pub`

// TODO this is needed because we want to combine the use_mapping Vec<String> with segs Vec<PathSegment>, but it might be better to just store use_mapping as Vec<PathSegment>.
#[derive(Debug, Clone)]
pub struct RustPathSegment {
    ident: String,
    turbofish: Vec<RustType>,
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
