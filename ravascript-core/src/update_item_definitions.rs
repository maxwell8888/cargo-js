use syn::{
    FnArg, GenericParam, ImplItem, ImplItemFn, Item, ItemConst, ItemEnum, ItemFn, ItemImpl,
    ItemStruct, ItemTrait, Member, Pat, ReturnType, Type, TypeParamBound, Visibility,
};
use tracing::debug_span;

use crate::{
    get_item_impl_unique_id,
    make_item_definitions::{self, ModuleMethods},
    parse_types_for_populate_item_definitions, RustGeneric, RustImplBlockSimple,
    RustImplItemItemNoJs, RustImplItemNoJs, RustType, RustTypeParam, RustTypeParamValue,
};

pub fn update_item_definitions(
    modules: Vec<make_item_definitions::ModuleData>,
) -> (Vec<ModuleData>, Vec<RustImplBlockSimple>) {
    // let global_data_copy = global_data.clone();

    let mut global_impl_blocks_simpl = Vec::new();
    for module in &modules {
        let module_path = module.path.clone();

        for (scope_id, item_impl) in &module.scoped_syn_impl_items {
            // Temporarily store impl block's type params on global data

            let impl_item_target_path = match &*item_impl.self_ty {
                Type::Path(type_path) => type_path
                    .path
                    .segments
                    .iter()
                    .map(|s| s.ident.to_string())
                    .collect::<Vec<_>>(),
                _ => todo!(),
            };

            let rust_impl_block_generics = item_impl
                .generics
                .params
                .iter()
                .filter_map(|gen| match gen {
                    GenericParam::Lifetime(_) => None,
                    GenericParam::Type(type_param) => Some(RustGeneric {
                        ident: type_param.ident.to_string(),
                        trait_bounds: type_param
                            .bounds
                            .iter()
                            .filter_map(|bound| {
                                // First lookup trait
                                match bound {
                                    TypeParamBound::Trait(trait_bound) => {
                                        let trait_path = trait_bound
                                            .path
                                            .segments
                                            .iter()
                                            .map(|seg| seg.ident.to_string());

                                        let (module_path, scope_id, trait_def) = modules
                                            .lookup_trait_definition_any_module(
                                                &module_path,
                                                &(!scope_id.is_empty()).then_some(scope_id.clone()),
                                                trait_path,
                                            );
                                        Some((module_path, scope_id, trait_def.name))
                                    }
                                    TypeParamBound::Lifetime(_) => None,
                                    TypeParamBound::Verbatim(_) => todo!(),
                                    _ => todo!(),
                                }
                            })
                            .collect::<Vec<_>>(),
                    }),
                    GenericParam::Const(_) => todo!(),
                })
                .collect::<Vec<_>>();

            let target_type_param = match &*item_impl.self_ty {
                Type::Path(type_path) => {
                    if type_path.path.segments.len() == 1 {
                        rust_impl_block_generics
                            .iter()
                            .find(|generic| {
                                let first_seg =
                                    type_path.path.segments.first().unwrap().ident.to_string();
                                generic.ident == first_seg
                            })
                            .cloned()
                    } else {
                        None
                    }
                }
                // TODO handle other `Type`s properly
                _ => None,
            };

            let trait_path_and_name = item_impl.trait_.as_ref().map(|(_, trait_, _)| {
                let (module_path, trait_scope_id, trait_def) = modules
                    .lookup_trait_definition_any_module(
                        &module_path,
                        &(!scope_id.is_empty()).then_some(scope_id.clone()),
                        trait_.segments.iter().map(|seg| seg.ident.to_string()),
                    );
                (module_path, trait_scope_id, trait_def.name)
            });

            let (target_rust_type, _is_target_type_param) =
                if let Some(target_type_param) = target_type_param {
                    (
                        RustType::TypeParam(RustTypeParam {
                            name: target_type_param.ident.clone(),
                            type_: RustTypeParamValue::Unresolved,
                        }),
                        true,
                    )
                } else {
                    // Get type of impl target

                    // dbg!(&module_path);
                    // dbg!(&temp_scope_id);
                    // dbg!(&impl_item_target_path);
                    let (target_item_module, resolved_scope_id, target_item) = modules
                        .lookup_item_definition_any_module_or_scope(
                            &module_path,
                            &(!scope_id.is_empty()).then_some(scope_id.clone()),
                            &impl_item_target_path,
                        );
                    // dbg!("here");
                    (
                        RustType::StructOrEnum(
                            target_item
                                .generics
                                .iter()
                                .map(|g| RustTypeParam {
                                    name: g.clone(),
                                    type_: RustTypeParamValue::Unresolved,
                                })
                                .collect::<Vec<_>>(),
                            target_item_module.clone(),
                            resolved_scope_id,
                            target_item.ident.to_string(),
                        ),
                        false,
                    )
                };

            // global_data.impl_block_target_type.pop();

            let rust_items = item_impl
                .items
                .iter()
                .map(|syn_item| {
                    let item_name = match syn_item {
                        ImplItem::Const(_) => todo!(),
                        ImplItem::Fn(impl_item_fn) => impl_item_fn.sig.ident.to_string(),
                        ImplItem::Type(_) => todo!(),
                        ImplItem::Macro(_) => todo!(),
                        ImplItem::Verbatim(_) => todo!(),
                        _ => todo!(),
                    };

                    let rust_impl_item_item = match syn_item {
                        ImplItem::Const(_) => todo!(),
                        ImplItem::Fn(impl_item_fn) => {
                            let impl_block_generics =
                                rust_impl_block_generics.iter().map(|g| g.ident.clone());
                            let fn_generics = impl_item_fn
                                .sig
                                .generics
                                .params
                                .iter()
                                .filter_map(|g| match g {
                                    GenericParam::Lifetime(_) => None,
                                    GenericParam::Type(type_param) => {
                                        Some(type_param.ident.to_string())
                                    }
                                    GenericParam::Const(_) => todo!(),
                                })
                                .collect::<Vec<_>>();
                            let combined_generics = impl_block_generics
                                .chain(fn_generics.iter().cloned())
                                .collect::<Vec<_>>();

                            let inputs_types = impl_item_fn
                                .sig
                                .inputs
                                .iter()
                                .map(|input| match input {
                                    FnArg::Receiver(_) => {
                                        // TODO need to actually parse the reciever to determine if it is boxed or a &mut so we can properly handle derefs
                                        // TODO need to ensure we are clear and consistent with the meaning of `RustType::ParentItem`
                                        (true, false, "self".to_string(), RustType::ParentItem)
                                    }
                                    FnArg::Typed(pat_type) => (
                                        false,
                                        match &*pat_type.pat {
                                            Pat::Ident(pat_ident) => pat_ident.mutability.is_some(),
                                            _ => todo!(),
                                        },
                                        match &*pat_type.pat {
                                            Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
                                            _ => todo!(),
                                        },
                                        parse_types_for_populate_item_definitions(
                                            &pat_type.ty,
                                            &combined_generics,
                                            &module_path,
                                            &(!scope_id.is_empty()).then_some(scope_id.clone()),
                                            &modules,
                                        ),
                                    ),
                                })
                                .collect::<Vec<_>>();

                            let return_type = match &impl_item_fn.sig.output {
                                ReturnType::Default => RustType::Unit,
                                ReturnType::Type(_, type_) => {
                                    parse_types_for_populate_item_definitions(
                                        type_,
                                        &combined_generics,
                                        &module_path,
                                        &(!scope_id.is_empty()).then_some(scope_id.clone()),
                                        &modules,
                                    )
                                }
                            };

                            // *scope_count += 1;
                            // scope_id.push(*scope_count);
                            // populate_impl_blocks_items_and_item_def_fields_stmts(
                            //     &impl_item_fn.block.stmts,
                            //     module,
                            //     global_data_copy,
                            //     module_path,
                            //     global_impl_blocks_simpl,
                            //     scope_id,
                            // );
                            // scope_id.pop();

                            let is_pub = match impl_item_fn.vis {
                                Visibility::Public(_) => true,
                                Visibility::Restricted(_) => todo!(),
                                Visibility::Inherited => false,
                            };
                            RustImplItemItemNoJs::Fn(
                                {
                                    if let Some(input) = impl_item_fn.sig.inputs.first() {
                                        match input {
                                            FnArg::Receiver(_) => true,
                                            FnArg::Typed(_) => false,
                                        }
                                    } else {
                                        false
                                    }
                                },
                                FnInfo {
                                    ident: item_name.clone(),
                                    is_pub,
                                    inputs_types,
                                    generics: fn_generics,
                                    return_type,
                                    syn: FnInfoSyn::Impl(impl_item_fn.clone()),
                                },
                            )
                        }
                        ImplItem::Type(_) => todo!(),
                        ImplItem::Macro(_) => todo!(),
                        ImplItem::Verbatim(_) => todo!(),
                        _ => todo!(),
                    };
                    RustImplItemNoJs {
                        ident: item_name.clone(),
                        item: rust_impl_item_item,
                        // syn_object: syn_item.clone(),
                    }
                })
                .collect();

            global_impl_blocks_simpl.push(RustImplBlockSimple {
                unique_id: get_item_impl_unique_id(
                    &module_path,
                    &(!scope_id.is_empty()).then_some(scope_id.clone()),
                    item_impl,
                ),
                generics: rust_impl_block_generics,
                trait_: trait_path_and_name,
                target: target_rust_type.clone(),
                rust_items,
            });
        }

        // for item in items {
        //     // dbg!("populate_impl_blocks_items");
        //     // println!("{}", quote! { #item });
        //     populate_impl_blocks_items_and_item_def_fields_individual(
        //         &item,
        //         module,
        //         &global_data_copy,
        //         &module_path,
        //         &mut global_data.impl_blocks_simpl,
        //         &mut scope_id,
        //         &mut scope_count,
        //     );
        // }

        // Add unique impl block ids to item_def.impl_block_ids
        // NOTE it is ok to do this in the same pass as populate_impl_blocks_items_and_item_def_fields because we are only relying on the traits defined in the impl block signature... NO it won't work because we are literally creating the `RustImplBlockSimple`s in populate_impl_blocks_items_and_item_def_fields so they won't all exist until the end of the loop over modules
        // populate_item_def_impl_blocks(&mut global_data);
        // update_item_def_block_ids(...
    }

    // TODO avoid clone
    let modules_copy = modules.clone();
    let new_modules = modules
        .into_iter()
        .map(|module| {
            debug_span!(
                "extract_data_populate_item_definitions module: {:?}",
                module_path = ?module.path
            );
            let module_path = module.path.clone();

            let updated_various_defs = update_various_def(
                module.various_definitions,
                &module_path,
                &None,
                &modules_copy,
            );

            let updated_scoped_various_defs = module
                .scoped_various_definitions
                .into_iter()
                .map(|(scope, various_def)| {
                    let scope_id = Some(scope.clone());
                    (
                        scope,
                        update_various_def(various_def, &module_path, &scope_id, &modules_copy),
                    )
                })
                .collect();

            ModuleData {
                name: module.name,
                path: module.path,
                pub_submodules: module.pub_submodules,
                private_submodules: module.private_submodules,
                pub_use_mappings: module.pub_use_mappings,
                private_use_mappings: module.private_use_mappings,
                resolved_mappings: module.resolved_mappings,
                various_definitions: updated_various_defs,
                items: module.items,
                scoped_various_definitions: updated_scoped_various_defs,
                scoped_syn_impl_items: module.scoped_syn_impl_items,
            }
        })
        .collect();

    (new_modules, global_impl_blocks_simpl)
}

fn update_various_def(
    various_definition: make_item_definitions::VariousDefintions,
    // global_data_copy: &make_item_definitions::GlobalData,
    module_path: &[String],
    current_scope: &Option<Vec<usize>>,
    modules: &[make_item_definitions::ModuleData],
) -> VariousDefintions {
    let new_const_defs = various_definition
        .consts
        .into_iter()
        .map(|const_def| {
            let rust_type = parse_types_for_populate_item_definitions(
                &const_def.syn_object.ty,
                &Vec::new(),
                module_path,
                current_scope,
                modules,
            );
            ConstDef {
                name: const_def.name,
                is_pub: const_def.is_pub,
                type_: rust_type,
                syn_object: const_def.syn_object,
            }
        })
        .collect();

    let new_item_defs = various_definition
        .item_definitons
        .into_iter()
        .map(|item_def| {
            let new_struct_or_enum_info = match item_def.struct_or_enum_info {
                make_item_definitions::StructOrEnumDefitionInfo::Struct(struct_def_info) => {
                    let fields = if struct_def_info.syn_object.fields.is_empty() {
                        StructFieldInfo::UnitStruct
                    } else if struct_def_info
                        .syn_object
                        .fields
                        .iter()
                        .next()
                        .unwrap()
                        .ident
                        .is_some()
                    {
                        StructFieldInfo::RegularStruct(
                            struct_def_info
                                .syn_object
                                .fields
                                .iter()
                                .map(|f| {
                                    (
                                        f.ident.as_ref().unwrap().to_string(),
                                        parse_types_for_populate_item_definitions(
                                            &f.ty,
                                            &item_def.generics,
                                            module_path,
                                            current_scope,
                                            modules,
                                        ),
                                    )
                                })
                                .collect::<Vec<_>>(),
                        )
                    } else {
                        StructFieldInfo::TupleStruct(
                            struct_def_info
                                .syn_object
                                .fields
                                .iter()
                                .map(|f| {
                                    parse_types_for_populate_item_definitions(
                                        &f.ty,
                                        &item_def.generics,
                                        module_path,
                                        current_scope,
                                        modules,
                                    )
                                })
                                .collect::<Vec<_>>(),
                        )
                    };
                    StructOrEnumDefitionInfo::Struct(StructDefinitionInfo {
                        fields,
                        syn_object: struct_def_info.syn_object,
                    })
                }
                make_item_definitions::StructOrEnumDefitionInfo::Enum(enum_def_info) => {
                    let members_for_scope = enum_def_info
                        .syn_object
                        .variants
                        .iter()
                        .map(|v| EnumVariantInfo {
                            ident: v.ident.to_string(),
                            inputs: v
                                .fields
                                .iter()
                                .map(|f| {
                                    let input_type = parse_types_for_populate_item_definitions(
                                        &f.ty,
                                        &item_def.generics,
                                        module_path,
                                        current_scope,
                                        modules,
                                    );
                                    match &f.ident {
                                        Some(input_name) => EnumVariantInputsInfo::Named {
                                            ident: input_name.to_string(),
                                            input_type,
                                        },
                                        None => EnumVariantInputsInfo::Unnamed(input_type),
                                    }
                                })
                                .collect::<Vec<_>>(),
                        })
                        .collect::<Vec<_>>();
                    StructOrEnumDefitionInfo::Enum(EnumDefinitionInfo {
                        members: members_for_scope,
                        syn_object: enum_def_info.syn_object,
                    })
                }
            };
            ItemDefinition {
                ident: item_def.ident,
                is_copy: item_def.is_copy,
                is_pub: item_def.is_pub,
                generics: item_def.generics,
                struct_or_enum_info: new_struct_or_enum_info,
                impl_block_ids: item_def.impl_block_ids,
            }
        })
        .collect();

    let new_fn_info = various_definition
        .fn_info
        .into_iter()
        .map(|fn_info| {
            let item_fn = match &fn_info.syn {
                make_item_definitions::FnInfoSyn::Standalone(item_fn) => item_fn,
                make_item_definitions::FnInfoSyn::Impl(_) => todo!(),
            };
            let inputs_types = item_fn
                .sig
                .inputs
                .iter()
                .map(|input| match input {
                    FnArg::Receiver(_) => {
                        // standalone functions cannot have self/receiver inputs
                        panic!();
                    }
                    FnArg::Typed(pat_type) => (
                        false,
                        match &*pat_type.pat {
                            Pat::Ident(pat_ident) => pat_ident.mutability.is_some(),
                            _ => todo!(),
                        },
                        match &*pat_type.pat {
                            Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
                            _ => todo!(),
                        },
                        parse_types_for_populate_item_definitions(
                            &pat_type.ty,
                            &fn_info.generics,
                            module_path,
                            current_scope,
                            modules,
                        ),
                    ),
                })
                .collect::<Vec<_>>();

            let return_type = match &fn_info.syn {
                make_item_definitions::FnInfoSyn::Standalone(item_fn) => {
                    match &item_fn.sig.output {
                        ReturnType::Default => RustType::Unit,
                        ReturnType::Type(_, type_) => parse_types_for_populate_item_definitions(
                            type_,
                            &fn_info.generics,
                            module_path,
                            current_scope,
                            modules,
                        ),
                    }
                }
                make_item_definitions::FnInfoSyn::Impl(_) => todo!(),
            };

            FnInfo {
                ident: fn_info.ident,
                is_pub: fn_info.is_pub,
                inputs_types,
                generics: fn_info.generics,
                return_type,
                syn: match fn_info.syn {
                    make_item_definitions::FnInfoSyn::Standalone(item_fn) => {
                        FnInfoSyn::Standalone(item_fn)
                    }
                    make_item_definitions::FnInfoSyn::Impl(impl_item_fn) => {
                        FnInfoSyn::Impl(impl_item_fn)
                    }
                },
            }
        })
        .collect();

    let new_trait_defs = various_definition
        .trait_definitons
        .into_iter()
        .map(|trait_def| {
            // Currently trait defs don't store any info other than the name, so we don't need to do anything
            RustTraitDefinition {
                name: trait_def.name,
                is_pub: trait_def.is_pub,
                syn: trait_def.syn,
            }
        })
        .collect();

    VariousDefintions {
        fn_info: new_fn_info,
        item_definitons: new_item_defs,
        consts: new_const_defs,
        trait_definitons: new_trait_defs,
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
    pub scoped_syn_impl_items: Vec<(Vec<usize>, ItemImpl)>,
}
impl ModuleData {
    pub fn item_defined_in_module(&self, use_private: bool, item: &str) -> bool {
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
