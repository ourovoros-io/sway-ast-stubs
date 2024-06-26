#[derive(Clone, Debug)]
pub struct AstModule {
    pub name: String,
    pub inner: sway_ast::Module,
}

#[derive(Clone, Debug)]
pub struct AstLibrary {
    pub name: String,
    pub modules: Vec<AstModule>,
}

#[derive(Clone, Debug)]
pub struct AstResolver {
    pub libraries: Vec<AstLibrary>,
}

impl AstResolver {
    pub fn new<P1: Clone + AsRef<std::path::Path>, P2: Clone + AsRef<std::path::Path>>(
        core_path: P1,
        std_path: P2,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        use sway_ast::keywords::{Keyword, Token};
        
        let core_preludes = vec![
            sway_ast::ItemKind::Use(sway_ast::ItemUse {
                visibility: None,
                use_token: sway_ast::keywords::UseToken::new(sway_types::Span::dummy()),
                root_import: None,
                tree: sway_ast::UseTree::Path {
                    prefix: sway_types::BaseIdent::new_no_span("core".into()),
                    double_colon_token: sway_ast::DoubleColonToken::default(),
                    suffix: Box::new(sway_ast::UseTree::Path {
                        prefix: sway_types::BaseIdent::new_no_span("prelude".into()),
                        double_colon_token: sway_ast::DoubleColonToken::default(),
                        suffix: Box::new(sway_ast::UseTree::Glob {
                            star_token: sway_ast::keywords::StarToken::new(sway_types::Span::dummy()),
                        }),
                    }),
                },
                semicolon_token: sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()),
            }),
        ];
    
        let std_preludes = vec![
            sway_ast::ItemKind::Use(sway_ast::ItemUse {
                visibility: None,
                use_token: sway_ast::keywords::UseToken::new(sway_types::Span::dummy()),
                root_import: None,
                tree: sway_ast::UseTree::Path {
                    prefix: sway_types::BaseIdent::new_no_span("core".into()),
                    double_colon_token: sway_ast::DoubleColonToken::default(),
                    suffix: Box::new(sway_ast::UseTree::Path {
                        prefix: sway_types::BaseIdent::new_no_span("prelude".into()),
                        double_colon_token: sway_ast::DoubleColonToken::default(),
                        suffix: Box::new(sway_ast::UseTree::Glob {
                            star_token: sway_ast::keywords::StarToken::new(sway_types::Span::dummy()),
                        }),
                    }),
                },
                semicolon_token: sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()),
            }),
            sway_ast::ItemKind::Use(sway_ast::ItemUse {
                visibility: None,
                use_token: sway_ast::keywords::UseToken::new(sway_types::Span::dummy()),
                root_import: None,
                tree: sway_ast::UseTree::Path {
                    prefix: sway_types::BaseIdent::new_no_span("std".into()),
                    double_colon_token: sway_ast::DoubleColonToken::default(),
                    suffix: Box::new(sway_ast::UseTree::Path {
                        prefix: sway_types::BaseIdent::new_no_span("prelude".into()),
                        double_colon_token: sway_ast::DoubleColonToken::default(),
                        suffix: Box::new(sway_ast::UseTree::Glob {
                            star_token: sway_ast::keywords::StarToken::new(sway_types::Span::dummy()),
                        }),
                    }),
                },
                semicolon_token: sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()),
            }),
        ];
    
        Ok(Self {
            libraries: vec![
                AstLibrary {
                    name: "core".into(),
                    modules: parse_ast_modules(core_path.clone(), core_path, core_preludes.as_slice())?,
                },
                AstLibrary {
                    name: "std".into(),
                    modules: parse_ast_modules(std_path.clone(), std_path, std_preludes.as_slice())?,
                },
            ],
        })
    }

    #[inline]
    pub fn resolve_module(&self, path_expr: &sway_ast::PathExpr) -> Option<&AstModule> {
        let module_name = path_expr.suffix.iter().map(|(_, x)| x.name.as_str()).collect::<Vec<_>>().join("::");
        let library = self.libraries.iter().find(|x| x.name == path_expr.prefix.name.as_str())?;
        library.modules.iter().find(|x| x.name == module_name)
    }

    #[inline]
    pub fn resolve_ty(&self, ty: &sway_ast::Ty) -> Option<&sway_ast::ItemKind> {
        match ty {
            sway_ast::Ty::Path(path_type) => self.resolve_path_type(path_type),

            sway_ast::Ty::Tuple(tuple) => {
                match &tuple.inner {
                    sway_ast::ty::TyTupleDescriptor::Nil => None,
                    sway_ast::ty::TyTupleDescriptor::Cons { .. } => todo!(),
                }
            }

            _ => todo!("{ty:#?}"),
        }
    }

    pub fn resolve_path_type(&self, path_type: &sway_ast::PathType) -> Option<&sway_ast::ItemKind> {
        let mut path_type_suffix = path_type.suffix.clone();
        let last_path_type = path_type_suffix.pop()?.1;

        let module_name = path_type_suffix.iter().map(|(_, x)| x.name.as_str()).collect::<Vec<_>>().join("::");
        
        let library = self.libraries.iter().find(|x| x.name == path_type.prefix.name.as_str())?;
        let module = library.modules.iter().find(|x| x.name == module_name)?;
        
        for item in &module.inner.items {
            match &item.value {
                sway_ast::ItemKind::Struct(sway_ast::ItemStruct { name, generics, .. })
                | sway_ast::ItemKind::Enum(sway_ast::ItemEnum { name, generics, .. })
                | sway_ast::ItemKind::Trait(sway_ast::ItemTrait { name, generics, .. }) => {
                    if *name != last_path_type.name {
                        continue;
                    }

                    match (generics.as_ref(), last_path_type.generics_opt.as_ref().map(|x| &x.1)) {
                        (None, None) => {
                            return Some(&item.value);
                        }

                        (Some(struct_generics), Some(path_generics)) => {
                            let mut idents = vec![];

                            for ident in &struct_generics.parameters.inner {
                                idents.push(ident);
                            }

                            let mut types = vec![];

                            for ty in &path_generics.parameters.inner {
                                types.push(ty);
                            }

                            //
                            // TODO: Do a stronger type check. We only check to see if they have the same length.
                            //

                            if idents.len() != types.len() {
                                continue;
                            }

                            return Some(&item.value);
                        }

                        _ => continue,
                    }
                }

                _ => {
                    //
                    // TODO: we should check for other things like constants and functions
                    //
                }
            }
        }

        None
    }
}

fn parse_ast_modules<P1: Clone + AsRef<std::path::Path>, P2: AsRef<std::path::Path>>(root_path: P1, path: P2, preludes: &[sway_ast::ItemKind]) -> Result<Vec<AstModule>, Box<dyn std::error::Error>> {
    let mut result = vec![];

    let root_path = root_path.as_ref().canonicalize()?;
    let root_path_string = root_path.to_string_lossy().to_string();

    let path = path.as_ref().canonicalize()?;

    for file in std::fs::read_dir(path).unwrap() {
        let file_path = file?.path();

        if file_path.is_dir() {
            result.extend(parse_ast_modules(root_path.clone(), file_path, preludes)?);
            continue;
        }

        if file_path
            .extension()
            .as_ref()
            .and_then(|x| x.to_str())
            .unwrap_or_default()
            != "sw"
        {
            continue;
        }

        let file = std::sync::Arc::<str>::from(std::fs::read_to_string(file_path.clone())?);
        let handler = sway_error::handler::Handler::default();
        let mut module = sway_parse::parse_file(&handler, file.clone(), None).unwrap();

        for prelude in preludes {
            module.value.items.push(sway_ast::attribute::Annotated {
                attribute_list: vec![],
                value: prelude.clone(),
            });
        }

        for item in module.value.items.iter_mut() {
            match &mut item.value {
                sway_ast::ItemKind::Submodule(_) => {}
                sway_ast::ItemKind::Use(_) => {}
                sway_ast::ItemKind::Struct(_) => {}
                sway_ast::ItemKind::Enum(_) => {}

                sway_ast::ItemKind::Fn(item_fn) => {
                    item_fn.body.inner.statements.clear();
                    item_fn.body.inner.final_expr_opt = None;
                }
                
                sway_ast::ItemKind::Trait(item_trait) => {
                    if let Some(trait_defs) = item_trait.trait_defs_opt.as_mut() {
                        for item_fn in trait_defs.inner.iter_mut() {
                            item_fn.value.body.inner.statements.clear();
                            item_fn.value.body.inner.final_expr_opt = None;
                        }
                    }
                }

                sway_ast::ItemKind::Impl(item_impl) => {
                    for impl_item in item_impl.contents.inner.iter_mut() {
                        match &mut impl_item.value {
                            sway_ast::ItemImplItem::Fn(item_fn) => {
                                item_fn.body.inner.statements.clear();
                                item_fn.body.inner.final_expr_opt = None;
                            }

                            sway_ast::ItemImplItem::Const(_) => {}
                            sway_ast::ItemImplItem::Type(_) => {}
                        }
                    }
                }

                sway_ast::ItemKind::Abi(item_abi) => {
                    if let Some(abi_defs) = item_abi.abi_defs_opt.as_mut() {
                        for item_fn in abi_defs.inner.iter_mut() {
                            item_fn.value.body.inner.statements.clear();
                            item_fn.value.body.inner.final_expr_opt = None;
                        }
                    }
                }

                sway_ast::ItemKind::Const(_) => {}
                sway_ast::ItemKind::Storage(_) => {}
                sway_ast::ItemKind::Configurable(_) => {}
                sway_ast::ItemKind::TypeAlias(_) => {}
                sway_ast::ItemKind::Error(_, _) => {}
            }
        }

        let module_name = std::path::PathBuf::from(
            file_path
                .to_string_lossy()
                .trim_start_matches(&root_path_string)
                .strip_suffix(".sw")
                .unwrap(),
        )
        .components()
        .map(|x| {
            x.as_os_str()
                .to_string_lossy()
                .trim_start_matches("/")
                .to_string()
        })
        .filter(|x| !x.is_empty())
        .collect::<Vec<_>>()
        .join("::");

        result.push(AstModule {
            name: module_name,
            inner: module.value,
        });
    }

    Ok(result)
}
