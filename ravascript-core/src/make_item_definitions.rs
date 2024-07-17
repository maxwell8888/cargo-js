use syn::{
    Expr, GenericParam, ImplItem, ImplItemFn, Item, ItemConst, ItemEnum, ItemFn, ItemStruct,
    ItemTrait, Member, Meta, Stmt, Type, Visibility,
};
use tracing::debug_span;

use crate::{extract_modules::ModuleDataFirstPass, RustType, RustTypeParam, RustTypeParamValue};

pub fn make_item_definitions(modules: Vec<ModuleDataFirstPass>) -> Vec<ModuleData> {
    // TODO the code for eg module.item_definitions.push(...) is a duplicated also for scope.item_definitons.push(...). Remove this duplication.

    // This is because parse_types_for_populate_item_definitions needs a access to .pub_definitions etc in global_data from `extract_data()` but we are taking an immutable ref first
    // We also need it for looking up trait definitions

    let mut new_modules = Vec::new();
    for module_first_pass in modules {
        debug_span!(
            "extract_data_populate_item_definitions module: {:?}",
            module_path = ?module_first_pass.path
        );

        let mut module = ModuleData {
            name: module_first_pass.name,
            // parent_name: module,
            path: module_first_pass.path,
            pub_submodules: module_first_pass.pub_submodules,
            private_submodules: module_first_pass.private_submodules,
            pub_use_mappings: module_first_pass.pub_use_mappings,
            private_use_mappings: module_first_pass.private_use_mappings,
            resolved_mappings: Vec::new(),
            // fn_info: Vec::new(),
            // item_definitons: Vec::new(),
            // consts: Vec::new(),
            // trait_definitons: Vec::new(),
            various_definitions: VariousDefintions::default(),
            items: module_first_pass.items,
            scoped_various_definitions: Vec::new(),
        };

        // TODO Gymnastics to reconcile needing to mutate 4 different vectors which are stored differently for modules and scopes. Should probably have `module.various_defs` and `scope.various_defs` fields
        let mut var_defs = VariousDefintions::default();
        let items = module.items.clone();
        let module_path = module.path.clone();
        let mut scope_id = Vec::new();

        let mut scope_count = 0;
        for item in &items {
            populate_item_definitions_items_individual_item(
                item,
                &module_path,
                &mut var_defs,
                &mut module,
                &mut scope_id,
                &mut scope_count,
            )
        }
        // module.various_definitions.fn_info.extend(var_defs.fn_info);
        // module
        //     .various_definitions
        //     .item_definitons
        //     .extend(var_defs.item_definitons);
        // module.various_definitions.consts.extend(var_defs.consts);
        // module
        //     .various_definitions
        //     .trait_definitons
        //     .extend(var_defs.trait_definitons);
        module.various_definitions = var_defs;
        new_modules.push(module);
    }
    new_modules
}

#[derive(Debug, Clone)]
pub struct ModuleData {
    pub name: String,
    // parent_name: Option<String>,
    /// NOTE the path includes the name of the module, eg the path to the crate module is ["crate"] not [].
    pub path: Vec<String>,
    // pub_definitions: Vec<String>,
    // private_definitions: Vec<String>,
    pub pub_submodules: Vec<String>,
    pub private_submodules: Vec<String>,
    /// (snake case item name, snake case use path)
    pub pub_use_mappings: Vec<(String, Vec<String>)>,
    pub private_use_mappings: Vec<(String, Vec<String>)>,
    /// Same format as use mapping but has absolute module path
    /// (snake case item name, snake case absolute module path)
    pub resolved_mappings: Vec<(String, Vec<String>)>,
    /// For recording information about the return type of fns
    ///
    /// TODO what if the fn is just imported from another module?
    ///
    /// We need:
    ///
    /// 1. A list of mappings from fn name to crate-global identifiers for all the fns available in the module (including imported ones), eg their absolute path or it's deduplicated name
    ///
    /// 2. A list of *all* fns in the crate
    ///
    /// Not easy to know which fns are available in module since some might be called like some_module::my_func() so we have to look at all Expr::Path in the code, not just use statements
    ///
    /// For now only lookup fns which are defined in module (including scopes)
    ///
    /// Should bear in mind how this might support generic associated fns eg T::default() - should be easy to store the extra info needed about the fn, or just store the whole ItemFn
    ///
    /// Module paths work as a unique key for top level items, but for fns in scopes need to just store them by name in a stack. The stack will need to be dynamic and follow the scopes, and pop stuff, so needs to happen during parsing, whereas top level fns get stored in the first pass ie extract_data()
    ///
    ///
    /// Top-level defined fns (possibly from other modules) available in this module, including cases like `use some_module; some_module::some_fn()`
    ///
    /// (<name>, <module path>)
    // // fn_info: Vec<(String, Vec<String>)>,
    // pub fn_info: Vec<FnInfo>,
    // pub item_definitons: Vec<ItemDefinition>,
    // /// (name, type, syn const)
    // pub consts: Vec<ConstDef>,
    // pub trait_definitons: Vec<RustTraitDefinition>,
    pub various_definitions: VariousDefintions,

    // We need this for extract_data_populate_item_definitions which happens after the modules ModuleData has been created by extract_data, but is populating the `ItemDefiitions` etc, and needs access to the original items in the module for this
    pub items: Vec<Item>,

    // (scope number, definitions)
    // scope number is eg [3,4,2] where this is the 3rd scope that appears within the module (not nested inside another scope, eg if the first 3 items are fns, this would be the body block of the 3rd fn, regardless of how many nested scoped there are in the first two fns), the 4th scope within that scope (same rules as before), and then finally the second scope within that scope
    // scoped_various_definitions: Vec<(Vec<usize>, VariousDefintions, Vec<RustImplBlockSimple>)>,
    pub scoped_various_definitions: Vec<(Vec<usize>, VariousDefintions)>,
}
impl ModuleData {
    /// NOTE the path includes the name of the module, eg the path to the crate module is ["crate"] not [].
    fn new(name: String, parent_name: Option<String>, path: Vec<String>) -> Self {
        ModuleData {
            name,
            // parent_name,
            path,
            // pub_definitions: Vec::new(),
            // private_definitions: Vec::new(),
            pub_submodules: Vec::new(),
            private_submodules: Vec::new(),
            pub_use_mappings: Vec::new(),
            private_use_mappings: Vec::new(),
            resolved_mappings: Vec::new(),
            // fn_info: Vec::new(),
            // item_definitons: Vec::new(),
            // trait_definitons: Vec::new(),
            // consts: Vec::new(),
            various_definitions: VariousDefintions::default(),
            items: Vec::new(),
            scoped_various_definitions: Vec::new(),
        }
    }
    pub fn item_defined_in_module(&self, use_private: bool, item: &str) -> bool {
        // let mut definitions = self.pub_definitions.iter();
        // if use_private {
        //     definitions
        //         .chain(self.private_definitions.iter())
        //         .any(|definition| definition == item)
        // } else {
        //     definitions.any(|definition| definition == item)
        // }
        self.various_definitions
            .item_definitons
            .iter()
            .filter_map(|item_def| (use_private || item_def.is_pub).then_some(&item_def.ident))
            .chain(
                self.various_definitions
                    .fn_info
                    .iter()
                    .filter_map(|fn_info| {
                        (use_private || fn_info.is_pub).then_some(&fn_info.ident)
                    }),
            )
            .chain(
                self.various_definitions
                    .trait_definitons
                    .iter()
                    .filter_map(|trait_def| {
                        (use_private || trait_def.is_pub).then_some(&trait_def.name)
                    }),
            )
            .chain(
                self.various_definitions
                    .consts
                    .iter()
                    .filter_map(|const_| (use_private || const_.is_pub).then_some(&const_.name)),
            )
            .any(|name| name == item)
    }
    pub fn path_starts_with_sub_module(&self, use_private: bool, item: &str) -> bool {
        let mut submodules = self.pub_submodules.iter();
        if use_private {
            submodules
                .chain(self.private_submodules.iter())
                .any(|submodule_name| submodule_name == item)
        } else {
            submodules.any(|submodule_name| submodule_name == item)
        }
    }

    pub fn lookup_item_def_known_module_assert_not_func2(
        &mut self,
        scope_id: &Option<Vec<usize>>,
        name: &str,
    ) -> &mut ItemDefinition {
        let scoped_item_def = scope_id
            .as_ref()
            .and_then(|scope_id| {
                self.scoped_various_definitions
                    .iter_mut()
                    .find(|svd| &svd.0 == scope_id)
            })
            .and_then(|svd| {
                svd.1
                    .item_definitons
                    .iter_mut()
                    .find(|item_def| item_def.ident == name)
            });
        let module_item_def = self
            .various_definitions
            .item_definitons
            .iter_mut()
            .find(|item_def| item_def.ident == name);

        scoped_item_def.or(module_item_def).unwrap()
    }

    pub fn lookup_const_known_module(
        &mut self,
        scope_id: &Option<Vec<usize>>,
        name: &str,
    ) -> &mut ConstDef {
        let scoped_const_def = scope_id
            .as_ref()
            .and_then(|scope_id| {
                self.scoped_various_definitions
                    .iter_mut()
                    .find(|svd| &svd.0 == scope_id)
            })
            .and_then(|svd| {
                svd.1
                    .consts
                    .iter_mut()
                    .find(|const_def| const_def.name == name)
            });
        let module_const_def = self
            .various_definitions
            .consts
            .iter_mut()
            .find(|const_def| const_def.name == name);

        scoped_const_def.or(module_const_def).unwrap()
    }

    pub fn lookup_fn_known_module(
        &mut self,
        scope_id: &Option<Vec<usize>>,
        name: &str,
    ) -> &mut FnInfo {
        let scoped_fn_info = scope_id
            .as_ref()
            .and_then(|scope_id| {
                self.scoped_various_definitions
                    .iter_mut()
                    .find(|svd| &svd.0 == scope_id)
            })
            .and_then(|svd| {
                svd.1
                    .fn_info
                    .iter_mut()
                    .find(|const_def| const_def.ident == name)
            });
        let module_fn_info = self
            .various_definitions
            .fn_info
            .iter_mut()
            .find(|const_def| const_def.ident == name);

        scoped_fn_info.or(module_fn_info).unwrap()
    }

    pub fn _lookup_trait_known_module(
        &mut self,
        scope_id: &Option<Vec<usize>>,
        name: &str,
    ) -> &mut RustTraitDefinition {
        let scoped_trait_def = scope_id
            .as_ref()
            .and_then(|scope_id| {
                self.scoped_various_definitions
                    .iter_mut()
                    .find(|svd| &svd.0 == scope_id)
            })
            .and_then(|svd| {
                svd.1
                    .trait_definitons
                    .iter_mut()
                    .find(|trait_def| trait_def.name == name)
            });
        let module_trait_def = self
            .various_definitions
            .trait_definitons
            .iter_mut()
            .find(|trait_def| trait_def.name == name);

        scoped_trait_def.or(module_trait_def).unwrap()
    }
}

trait GetModule {
    fn get_mut(&mut self, module_path: &[String]) -> &mut ModuleData;
}
impl GetModule for Vec<ModuleData> {
    fn get_mut(&mut self, module_path: &[String]) -> &mut ModuleData {
        self.iter_mut().find(|m| m.path == module_path).unwrap()
    }
}

#[derive(Debug, Clone, Default)]
pub struct VariousDefintions {
    pub fn_info: Vec<FnInfo>,
    pub item_definitons: Vec<ItemDefinition>,
    pub consts: Vec<ConstDef>,
    pub trait_definitons: Vec<RustTraitDefinition>,
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub name: String,
    pub is_pub: bool,
    pub type_: RustType,
    pub syn_object: ItemConst,
}

/// Not just for methods, can also be an enum variant with no inputs
#[derive(Debug, Clone)]
pub struct FnInfo {
    // TODO No point storing all the info like inputs and return types separately, as these need to be stored on RustType::Fn anyway for eg closures where we won't be storing a fn info?? Keep both for now and revisit later. Note fns idents can just appear in the code and be called whereas a closure will be a var which already has a type.
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
}

#[derive(Debug, Clone)]
pub enum FnInfoSyn {
    Standalone(ItemFn),
    Impl(ImplItemFn),
}

impl FnInfo {
    pub fn attempt_to_resolve_type_params_using_arg_types(
        &self,
        args: &[RustType],
    ) -> Vec<RustTypeParam> {
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
                        RustTypeParamValue::RustType(Box::new(matched_arg_rust_type))
                    } else {
                        RustTypeParamValue::Unresolved
                    };

                RustTypeParam {
                    name: g.clone(),
                    type_: rust_type_param_value,
                }
            })
            .collect::<Vec<_>>()
    }
}

fn populate_item_definitions_items_individual_item(
    item: &Item,
    // global_data: &GlobalData,
    module_path: &[String],
    // These `various_defs` are will either be added to a module if this fn is called when iterating over module level items, or a scope is it is called when iterating over stmts
    various_defs: &mut VariousDefintions,
    module: &mut ModuleData,
    scope_id: &mut Vec<usize>,
    scope_count: &mut usize,
) {
    match item {
        Item::Const(item_const) => {
            let const_name = item_const.ident.to_string();

            let is_pub = match item_const.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };
            various_defs.consts.push(ConstDef {
                name: const_name,
                is_pub,
                type_: RustType::Todo,
                syn_object: item_const.clone(),
            });
        }
        Item::Enum(item_enum) => {
            let enum_name = item_enum.ident.to_string();

            // Make ItemDefinition
            let generics = item_enum
                .generics
                .params
                .iter()
                .map(|p| match p {
                    GenericParam::Lifetime(_) => todo!(),
                    GenericParam::Type(type_param) => type_param.ident.to_string(),
                    GenericParam::Const(_) => todo!(),
                })
                .collect::<Vec<_>>();
            let members_for_scope = item_enum
                .variants
                .iter()
                .map(|v| EnumVariantInfo {
                    ident: v.ident.to_string(),
                    inputs: Vec::new(),
                })
                .collect::<Vec<_>>();

            let is_copy = item_enum.attrs.iter().any(|attr| match &attr.meta {
                Meta::Path(_) => todo!(),
                Meta::List(meta_list) => {
                    let segs = &meta_list.path.segments;
                    if segs.len() == 1 && segs.first().unwrap().ident == "derive" {
                        let tokens = format!("({})", meta_list.tokens);
                        let trait_tuple = syn::parse_str::<syn::TypeTuple>(&tokens).unwrap();
                        trait_tuple.elems.iter().any(|elem| match elem {
                            Type::Path(type_path) => {
                                let segs = &type_path.path.segments;
                                // TODO `Copy` could have been shadowed to need to do a proper lookup for trait with name `Copy` to check whether it is std::Copy or not.
                                segs.len() == 1 && segs.first().unwrap().ident == "Copy"
                            }
                            _ => todo!(),
                        })
                    } else {
                        false
                    }
                }
                Meta::NameValue(_) => todo!(),
            });

            let is_pub = match item_enum.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };
            various_defs.item_definitons.push(ItemDefinition {
                ident: enum_name,
                is_copy,
                is_pub,
                generics,
                struct_or_enum_info: StructOrEnumDefitionInfo::Enum(EnumDefinitionInfo {
                    members: members_for_scope,
                    syn_object: item_enum.clone(),
                }),
                impl_block_ids: Vec::new(),
            });
        }
        Item::ExternCrate(_) => todo!(),
        Item::Fn(item_fn) => {
            let generics = item_fn
                .sig
                .generics
                .params
                .iter()
                .filter_map(|g| match g {
                    GenericParam::Lifetime(_) => None,
                    GenericParam::Type(type_param) => Some(type_param.ident.to_string()),
                    GenericParam::Const(_) => todo!(),
                })
                .collect::<Vec<_>>();

            let is_pub = match item_fn.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };
            various_defs.fn_info.push(FnInfo {
                ident: item_fn.sig.ident.to_string(),
                is_pub,
                inputs_types: Vec::new(),
                generics,
                // return_type: RustType::Uninit,
                return_type: RustType::Todo,
                syn: FnInfoSyn::Standalone(item_fn.clone()),
            });

            // Get scoped definitions

            // dbg!("populate_item_definitions_items");
            // let sig = &item_fn.sig;
            // println!("{}", quote! { #sig });
            // dbg!(scope_count);

            // let mut itemms = Vec::new();
            // for stmt in item_fn.block.stmts.clone().into_iter() {
            //     append_items_from_stmt(&mut itemms, stmt);
            // }

            // *scope_number.last_mut().unwrap() += 1;

            // dbg!("push item_fn scope");
            // let sig = &item_fn.sig;
            // println!("{}", quote! { #item_fn });
            // dbg!(&scope_count);
            // dbg!(&scope_id);
            *scope_count += 1;
            let mut scoped_various_defs = VariousDefintions::default();

            // We are now processing the items within the fn block, so are no longer at the module level and now in a scope (or in a new child scope), so push a new scope level
            scope_id.push(*scope_count);
            // dbg!(&scope_count);
            // dbg!(&scope_id);
            // dbg!("populate item fn");
            // dbg!(item_fn.sig.ident.to_string());
            // dbg!(&scope_id);
            // populate_item_definitions_items(
            //     &itemms,
            //     global_data,
            //     module_path,
            //     &mut scoped_various_defs,
            //     module,
            //     scope_id,
            // );
            populate_item_definitions_stmts(
                &item_fn.block.stmts,
                // global_data,
                module_path,
                &mut scoped_various_defs,
                module,
                scope_id,
            );

            // TODO it seems like it would be more simple and intuitive to create the `VariousDefinitions` scope and add it to the `module.scoped_various_definitions` in the `populate_item_definitions_stmts()` fn rather than doing it outside like this. Will wait until we have all tests passing before attempting to make this change.
            module.scoped_various_definitions.push((
                scope_id.clone(),
                scoped_various_defs,
                // Vec::new(),
            ));
            // dbg!("pop item_fn scope");
            scope_id.pop();
            // dbg!(&scope_count);
            // dbg!(&scope_id);
        }
        Item::ForeignMod(_) => todo!(),
        Item::Impl(item_impl) => {
            // TODO IMPORTANT currently we are adding top level impl blocks to `global_data.impl_blocks` in handle_item_impl(). It would be better to push (non-scoped) impl blocks here, so that they are already available if a method defined on the impl is called before the impl block itself is reached/parsed by `handle_item_impl()`. However we still need to find a way to solve this problem for the scoped impl blocks anyway. Leave it as is for now until we do some refactoring and deduplication, to avoid need to repeat a bunch of code here.

            // TODO also need to go through scopes in impl fns, like with standalone fns
            for item in &item_impl.items {
                match item {
                    ImplItem::Const(_) => todo!(),
                    ImplItem::Fn(impl_item_fn) => {
                        // dbg!("push item_impl_fn scope");
                        // dbg!(module_path);
                        // let sig = &impl_item_fn.sig;
                        // println!("{}", quote! { #impl_item_fn });
                        // dbg!(&scope_count);
                        // dbg!(&scope_id);
                        *scope_count += 1;
                        scope_id.push(*scope_count);
                        // dbg!(&scope_count);
                        // dbg!(&scope_id);
                        let mut scoped_various_defs = VariousDefintions::default();
                        populate_item_definitions_stmts(
                            &impl_item_fn.block.stmts,
                            // global_data,
                            module_path,
                            &mut scoped_various_defs,
                            module,
                            scope_id,
                        );
                        module.scoped_various_definitions.push((
                            scope_id.clone(),
                            scoped_various_defs,
                            // Vec::new(),
                        ));
                        // dbg!("pop item_impl_fn scope");
                        scope_id.pop();
                        // dbg!(&scope_count);
                        // dbg!(&scope_id);
                    }
                    ImplItem::Type(_) => todo!(),
                    ImplItem::Macro(_) => todo!(),
                    ImplItem::Verbatim(_) => todo!(),
                    _ => todo!(),
                }
            }
        }
        Item::Macro(_) => {}
        // We have already split up the modules in individual `ModuleData`s (which we are currently iterating through) so should ignore `Item::Mod`s
        Item::Mod(_) => {}
        Item::Static(_) => todo!(),
        Item::Struct(item_struct) => {
            // dbg!("populate_item_definitions_items_individual_item");
            // println!("{}", quote! { #item_struct });
            let struct_name = item_struct.ident.to_string();

            // Make ItemDefinition
            let generics = item_struct
                .generics
                .params
                .iter()
                .filter_map(|p| match p {
                    GenericParam::Lifetime(_) => None,
                    GenericParam::Type(type_param) => Some(type_param.ident.to_string()),
                    GenericParam::Const(_) => todo!(),
                })
                .collect::<Vec<_>>();

            let generics_type_params = generics
                .iter()
                .map(|name| RustTypeParam {
                    name: name.clone(),
                    type_: RustTypeParamValue::Unresolved,
                })
                .collect::<Vec<_>>();

            let fields = if item_struct.fields.is_empty() {
                StructFieldInfo::UnitStruct
            } else if item_struct.fields.iter().next().unwrap().ident.is_some() {
                StructFieldInfo::RegularStruct(Vec::new())
            } else {
                StructFieldInfo::TupleStruct(Vec::new())
            };

            let is_copy = item_struct.attrs.iter().any(|attr| match &attr.meta {
                Meta::Path(_) => todo!(),
                Meta::List(meta_list) => {
                    let segs = &meta_list.path.segments;
                    if segs.len() == 1 && segs.first().unwrap().ident == "derive" {
                        let tokens = format!("({},)", meta_list.tokens);
                        // NOTE can't parse as syn::TypeTuple because eg (Default) is not a tuple, only len > 1 like (Default, Debug)
                        let trait_tuple = syn::parse_str::<syn::TypeTuple>(&tokens).unwrap();
                        trait_tuple.elems.iter().any(|elem| match elem {
                            Type::Path(type_path) => {
                                let segs = &type_path.path.segments;
                                // TODO `Copy` could have been shadowed to need to do a proper lookup for trait with name `Copy` to check whether it is std::Copy or not.
                                segs.len() == 1 && segs.first().unwrap().ident == "Copy"
                            }
                            _ => todo!(),
                        })
                    } else {
                        false
                    }
                }
                Meta::NameValue(_) => todo!(),
            });

            let is_pub = match item_struct.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };
            various_defs.item_definitons.push(ItemDefinition {
                ident: item_struct.ident.to_string(),
                is_pub,
                is_copy,
                generics,
                struct_or_enum_info: StructOrEnumDefitionInfo::Struct(StructDefinitionInfo {
                    fields,
                    syn_object: item_struct.clone(),
                }),
                impl_block_ids: Vec::new(),
            });
        }
        Item::Trait(item_trait) => {
            let is_pub = match item_trait.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };
            various_defs.trait_definitons.push(RustTraitDefinition {
                name: item_trait.ident.to_string(),
                is_pub,
                syn: item_trait.clone(),
            })
        }
        Item::TraitAlias(_) => todo!(),
        Item::Type(_) => todo!(),
        Item::Union(_) => todo!(),
        Item::Use(_) => {}
        Item::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}

fn populate_item_definitions_stmts(
    stmts: &[Stmt],
    // global_data: &GlobalData,
    module_path: &[String],
    current_scope_various_defs: &mut VariousDefintions,
    module: &mut ModuleData,
    scope_id: &mut Vec<usize>,
) {
    let mut scope_count = 0;

    for stmt in stmts {
        match stmt {
            Stmt::Local(local) => {
                let init = local.init.as_ref().unwrap();
                populate_item_definitions_expr(
                    &init.expr,
                    // global_data,
                    module_path,
                    // current_scope_various_defs,
                    module,
                    scope_id,
                    &mut scope_count,
                    false,
                );
            }
            Stmt::Item(item) => populate_item_definitions_items_individual_item(
                item,
                // global_data,
                module_path,
                current_scope_various_defs,
                module,
                scope_id,
                &mut scope_count,
            ),
            Stmt::Expr(expr, _) => populate_item_definitions_expr(
                expr,
                // global_data,
                module_path,
                // current_scope_various_defs,
                module,
                scope_id,
                &mut scope_count,
                false,
            ),
            Stmt::Macro(stmt_macro) => {
                if stmt_macro.mac.path.segments.len() == 1
                    && stmt_macro.mac.path.segments.first().unwrap().ident == "assert"
                {
                    let input = stmt_macro.mac.tokens.clone().to_string();
                    let condition_expr = syn::parse_str::<syn::Expr>(&input).unwrap();
                    populate_item_definitions_expr(
                        &condition_expr,
                        // global_data,
                        module_path,
                        // current_scope_various_defs,
                        module,
                        scope_id,
                        &mut scope_count,
                        false,
                    )
                } else {
                    dbg!(&stmt_macro.mac);
                    todo!();
                }
            }
        }
    }
}

fn populate_item_definitions_expr(
    expr: &Expr,
    // global_data: &GlobalData,
    module_path: &[String],
    // various_defs: &mut VariousDefintions,
    module: &mut ModuleData,
    scope_id: &mut Vec<usize>,
    // NOTE when we are iterating through some stmts for a given scope like a fn, we need to be able to increment the scope count each time we come across an expression that create a new child scope eg a block. So for a fn body with 3 blocks, by the time we reach the final block, the scope_count will have been incremented to 2 (0 based) so we can then push the scope_count onto scope_id to create a new scope for processing the statement in that block.
    scope_count: &mut usize,
    // In some cases, eg the body of a match arm like `Foo(x) => x`, we need to ensure a new scope is created even though we wouldn't normally create one for the give `Expr` variant, ie an `Expr::Path` in this case.
    // NOTE both here and in the JS parsing code, we create a new scope even if we don't need to eg for match arms like `Foo => 5`.
    force_new_scope: bool,
) {
    fn forced_inc_scope_count_and_id_and_push_empty_scope(
        force_new_scope: bool,
        scope_count: &mut usize,
        scope_id: &mut Vec<usize>,
        module: &mut ModuleData,
    ) {
        if force_new_scope {
            *scope_count += 1;
            scope_id.push(*scope_count);

            let empty_various_defs = VariousDefintions::default();
            module
                .scoped_various_definitions
                .push((scope_id.clone(), empty_various_defs));
        }
    }
    fn drop_forced_empty_scope(force_new_scope: bool, scope_id: &mut Vec<usize>) {
        if force_new_scope {
            scope_id.pop();
        }
    }

    // We only want to create a new scope for the below expressions
    // TODO IMPORTANT call `make_forced_empty_scope` for all variants
    match expr {
        Expr::Array(_) => {}
        Expr::Assign(_) => {}
        Expr::Async(expr_async) => {
            // This is identical to the handling of Expr::Block but we have to get the block from the `ExprAsync` first
            *scope_count += 1;
            scope_id.push(*scope_count);

            let mut scoped_various_defs = VariousDefintions::default();
            populate_item_definitions_stmts(
                &expr_async.block.stmts,
                // global_data,
                module_path,
                &mut scoped_various_defs,
                module,
                scope_id,
            );

            module.scoped_various_definitions.push((
                scope_id.clone(),
                scoped_various_defs,
                // Vec::new(),
            ));

            scope_id.pop();
        }
        Expr::Await(_) => {}
        Expr::Binary(_) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            // TODO call populate_item_definitions_expr for side
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Block(expr_block) => {
            *scope_count += 1;
            scope_id.push(*scope_count);

            let mut scoped_various_defs = VariousDefintions::default();
            populate_item_definitions_stmts(
                &expr_block.block.stmts,
                // global_data,
                module_path,
                &mut scoped_various_defs,
                module,
                scope_id,
            );

            module.scoped_various_definitions.push((
                scope_id.clone(),
                scoped_various_defs,
                // Vec::new(),
            ));

            scope_id.pop();
        }
        Expr::Break(_) => {}
        Expr::Call(_) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            // TODO call populate_item_definitions_expr for each argument?
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Cast(_) => {}
        Expr::Closure(expr_closure) => {
            populate_item_definitions_expr(
                &expr_closure.body,
                // global_data,
                module_path,
                module,
                scope_id,
                scope_count,
                true,
            );
        }
        Expr::Const(_) => {}
        Expr::Continue(_) => {}
        Expr::Field(_) => {}
        Expr::ForLoop(_) => {}
        Expr::Group(_) => {}
        Expr::If(_) => {}
        Expr::Index(_) => {}
        Expr::Infer(_) => {}
        Expr::Let(_) => {}
        Expr::Lit(_) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Loop(_) => {}
        Expr::Macro(_) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Match(expr_match) => {
            // We wouldn't normally create a new scope for a match expression but if it is the body of eg a closure or a another match expression's arm body, then force_new_scope will be true and we must create an empty scope (NOTE this is different to the arm body scopes which are created below, this is a single empty scope for the entire match expression)
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );

            populate_item_definitions_expr(
                &expr_match.expr,
                // global_data,
                module_path,
                module,
                scope_id,
                scope_count,
                false,
            );
            // TODO We count in reverse to match the JS parsing but should probably try and fix this so we don't have to remember that match arm body scopes are counted backwards
            for arm in expr_match.arms.iter().rev() {
                // In this case, even if the arm body is simply a path like `x`, we still need to create a scope because the `x` can be an argument of eg the enum variant for this match arm.
                populate_item_definitions_expr(
                    &arm.body,
                    // global_data,
                    module_path,
                    module,
                    scope_id,
                    scope_count,
                    true,
                );
            }
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::MethodCall(expr_method_call) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            populate_item_definitions_expr(
                &expr_method_call.receiver,
                // global_data,
                module_path,
                module,
                scope_id,
                scope_count,
                false,
            );
            for expr in &expr_method_call.args {
                populate_item_definitions_expr(
                    expr,
                    // global_data,
                    module_path,
                    module,
                    scope_id,
                    scope_count,
                    false,
                );
            }
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Paren(_) => {}
        Expr::Path(_) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Range(_) => {}
        Expr::Reference(_) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Repeat(_) => {}
        Expr::Return(_) => {}
        Expr::Struct(_) => {}
        Expr::Try(_) => {}
        Expr::TryBlock(_) => {}
        Expr::Tuple(_) => {}
        Expr::Unary(expr_unary) => {
            // TODO I think it is correct that we don't need to call `forced_inc_scope_count_and_id_and_push_empty_scope` and can just pass `force_new_scope` down to the target expression to create a new scope if necessary? If this is correct then `Expr::Reference` should be the same as similarly to unary you can't have a reference with no target
            // forced_inc_scope_count_and_id_and_push_empty_scope(
            //     force_new_scope,
            //     scope_count,
            //     scope_id,
            //     module,
            // );
            populate_item_definitions_expr(
                &expr_unary.expr,
                module_path,
                module,
                scope_id,
                scope_count,
                force_new_scope,
                // false,
            );
            // drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Unsafe(_) => {}
        Expr::Verbatim(_) => {}
        Expr::While(_) => {}
        Expr::Yield(_) => {}
        _ => {}
    }
}

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
    /// (unique impl id)
    pub impl_block_ids: Vec<String>,
}
impl ItemDefinition {
    pub fn get_type(&self, field_member: &Member) -> RustType {
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
                        (field_name == &field_member_name).then_some(field_type)
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
    pub name: String,
    pub is_pub: bool,
    // impl_items:
    pub syn: ItemTrait,
}

// We have some kind of usage of the struct/enum, eg `let foo = Foo::Bar(5)` and want to check if the struct/enum is/has generic(s) and if so is eg the input to variant `Bar` one of those generic(s). For now just store the whole ItemStruct/ItemEnum and do the checking each time from wherever eg `Foo::Bar(5)` is.
// generic could be determined by: input for enum variant, input to method, field value of struct constructor.
// #[derive(Debug, Clone)]
// struct ItemDefinition {
//     ident: String,
//     /// so this includes methods, fields, enum variants, etc, basically anything that can come after . or :: so we will usually get a different type from the "parent"
//     ///
//     /// When look up return type of a member and the return type is Self, then need to return this StructOrEnum, as a RustType, possibly first updating the generics based on eg the types used in the args of the assignment variable's type.
//     ///
//     /// NOTE we might have nested generics so need to be careful to keep them separate and not flatten them so they can be resolved later?
//     /// like `let var = Foo<Bar<Baz<Option(i32)>>> = Foo::new()`?????
//     /// Don't need to store member info on an instance?
//     // members: Vec<MemberInfo>,
//     // members: Vec<ImplItem>,
//     generics: Vec<RustTypeParam>,
//     syn_object: StructOrEnumSynObject,
// }
impl ItemDefinition {
    // /// Update generics based on types of args
    // ///
    // /// For all the generics of the struct/enum...
    // /// ...for enum check if any of the arguments to any of the variants are the generic type...
    // /// ...(if) we found a generic so now we need to find the type of the argument being passed and we will have the full `MyEnum<FoundGeneric>` type
    // fn attempt_to_resolve_generics(
    //     &self,
    //     field_or_variant_name: &String,
    //     args: &Vec<(JsExpr, RustType)>,
    // ) -> Vec<RustTypeParam> {
    //     let mut possibly_resolved_generics = Vec::new();
    //     for generic in &self.generics {
    //         match &self.struct_or_enum_info {
    //             StructOrEnumDefitionInfo::Struct(_) => todo!(),
    //             StructOrEnumDefitionInfo::Enum(enum_def_info) => {
    //                 let item_enum = &enum_def_info.syn_object;
    //                 // ...for enum check if any of the arguments to any of the variants are the generic type...
    //                 for v in &item_enum.variants {
    //                     if v.ident == field_or_variant_name {
    //                         match &v.fields {
    //                             Fields::Named(fields_named) => {
    //                                 for (i, field) in fields_named.named.iter().enumerate() {
    //                                     match &field.ty {
    //                                         Type::Path(type_path) => {
    //                                             if type_path.path.segments.first().unwrap().ident
    //                                                 == generic
    //                                             {
    //                                                 // ...we found a generic so now we need to find the type of the argument being passed and we will have the full `MyEnum<FoundGeneric>` type
    //                                                 let (_js_expr, rust_type) = args[i].clone();
    //                                                 possibly_resolved_generics.push(
    //                                                     RustTypeParam {
    //                                                         name: generic.clone(),
    //                                                         type_: RustTypeParamValue::RustType(
    //                                                             Box::new(rust_type),
    //                                                         ),
    //                                                     },
    //                                                 );
    //                                                 continue;
    //                                             }
    //                                         }
    //                                         Type::Verbatim(_) => todo!(),
    //                                         _ => todo!(),
    //                                     }
    //                                 }
    //                             }
    //                             Fields::Unnamed(fields_unnamed) => {
    //                                 for (i, field) in fields_unnamed.unnamed.iter().enumerate() {
    //                                     match &field.ty {
    //                                         Type::Path(type_path) => {
    //                                             if type_path.path.segments.first().unwrap().ident
    //                                                 == generic
    //                                             {
    //                                                 // ...we found a generic so now we need to find the type of the argument being passed and we will have the full `MyEnum<FoundGeneric>` type
    //                                                 let (_js_expr, rust_type) = args[i].clone();
    //                                                 possibly_resolved_generics.push(
    //                                                     RustTypeParam {
    //                                                         name: generic.clone(),
    //                                                         type_: RustTypeParamValue::RustType(
    //                                                             Box::new(rust_type),
    //                                                         ),
    //                                                     },
    //                                                 );
    //                                                 continue;
    //                                             }
    //                                         }
    //                                         Type::Verbatim(_) => todo!(),
    //                                         _ => todo!(),
    //                                     }
    //                                 }
    //                             }
    //                             Fields::Unit => todo!(),
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //         possibly_resolved_generics.push(RustTypeParam {
    //             name: generic.clone(),
    //             type_: RustTypeParamValue::Unresolved,
    //         });
    //     }
    //     possibly_resolved_generics
    // }
}
