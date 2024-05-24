use proc_macro2::TokenStream;
use quote::quote;

// NOTE: We use the `include!` macro because the `AstResolver` structure is used by both the build script and the crate itself.
include!("src/resolver.rs");

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("cargo::rerun-if-changed=build.rs");

    if !std::path::Path::new("./sway/").exists() {
        std::process::Command::new("git")
            .args(&["clone", "https://github.com/FuelLabs/sway.git"])
            .spawn()?
            .wait_with_output()?;
    }

    let ast_resolver = AstResolver {
        libraries: vec![
            AstLibrary {
                name: "core".into(),
                modules: parse_ast_modules("./sway/sway-lib-core/src", "./sway/sway-lib-core/src")?,
            },
            AstLibrary {
                name: "std".into(),
                modules: parse_ast_modules("./sway/sway-lib-std/src", "./sway/sway-lib-std/src")?,
            },
        ],
    };

    let mut libraries = vec![];

    for library in ast_resolver.libraries.iter() {
        libraries.push(generate_library_code(library));
    }

    let generated = quote::quote! {
        impl Default for AstResolver {
            fn default() -> Self {
                use sway_ast::keywords::{Keyword, Token};

                // HACK: Ensure we have at least 64MB stack space available
                stacker::grow(64 * 1024 * 1024, || Self {
                    libraries: vec![#(#libraries),*],
                })
            }
        }
    };

    std::fs::write("src/default.rs", generated.to_string())?;

    Ok(())
}

fn generate_library_code(library: &AstLibrary) -> TokenStream {
    let name = library.name.clone();
    let modules = library
        .modules
        .iter()
        .map(|x| generate_module_code(x))
        .collect::<Vec<_>>();

    quote!(AstLibrary {
        name: #name.into(),
        modules: vec![#(#modules),*],
    })
}

fn generate_module_code(module: &AstModule) -> TokenStream {
    let name = module.name.clone();
    let kind = generate_module_kind_code(&module.inner.kind);
    let items = module
        .inner
        .items
        .iter()
        .map(|x| generate_item_kind_code(x))
        .collect::<Vec<_>>();

    quote!(AstModule {
        name: #name.into(),
        inner: sway_ast::Module {
            kind: #kind,
            semicolon_token: sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()),
            items: vec![#(#items),*],
        },
    })
}

fn generate_module_kind_code(kind: &sway_ast::ModuleKind) -> TokenStream {
    let (module_kind, token_field, token_kind) = match kind {
        sway_ast::ModuleKind::Script { .. } => {
            (quote!(Script), quote!(script_token), quote!(ScriptToken))
        }
        sway_ast::ModuleKind::Contract { .. } => (
            quote!(Contract),
            quote!(contract_token),
            quote!(ContractToken),
        ),
        sway_ast::ModuleKind::Predicate { .. } => (
            quote!(Predicate),
            quote!(predicate_token),
            quote!(PredicateToken),
        ),
        sway_ast::ModuleKind::Library { .. } => {
            (quote!(Library), quote!(library_token), quote!(LibraryToken))
        }
    };

    quote!(sway_ast::ModuleKind::#module_kind {
        #token_field: sway_ast::keywords::#token_kind::new(
            sway_types::Span::dummy()
        )
    })
}

fn generate_item_kind_code(
    item: &sway_ast::attribute::Annotated<sway_ast::ItemKind>,
) -> TokenStream {
    let attribute_list = item
        .attribute_list
        .iter()
        .map(|x| generate_attribute_decl_code(x))
        .collect::<Vec<_>>();

    let (kind, value) = match &item.value {
        sway_ast::ItemKind::Submodule(item_submodule) => (quote!(Submodule), generate_item_submodule_code(item_submodule)),
        sway_ast::ItemKind::Use(item_use) => (quote!(Use), generate_item_use_code(item_use)),
        sway_ast::ItemKind::Struct(item_struct) => (quote!(Struct), generate_item_struct_code(item_struct)),
        sway_ast::ItemKind::Enum(item_enum) => (quote!(Enum), generate_item_enum_code(item_enum)),
        sway_ast::ItemKind::Fn(item_fn) => (quote!(Fn), generate_item_fn_code(item_fn)),
        sway_ast::ItemKind::Trait(item_trait) => (quote!(Trait), generate_item_trait_code(item_trait)),
        sway_ast::ItemKind::Impl(item_impl) => (quote!(Impl), generate_item_impl_code(item_impl)),
        sway_ast::ItemKind::Abi(item_abi) => (quote!(Abi), generate_item_abi_code(item_abi)),
        sway_ast::ItemKind::Const(item_const) => (quote!(Const), generate_item_const_code(item_const)),
        sway_ast::ItemKind::Storage(item_storage) => (quote!(Storage), generate_item_storage_code(item_storage)),
        sway_ast::ItemKind::Configurable(item_configurable) => (quote!(Configurable), generate_item_configurable_code(item_configurable)),
        sway_ast::ItemKind::TypeAlias(item_type_alias) => (quote!(TypeAlias), generate_item_type_alias_code(item_type_alias)),
        
        sway_ast::ItemKind::Error(_, _) => {
            panic!("An error occurred while parsing Sway AST")
        }
    };

    quote!(sway_ast::attribute::Annotated {
        attribute_list: vec![#(#attribute_list),*],
        value: sway_ast::ItemKind::#kind(#value),
    })
}

fn generate_base_ident_code(base_ident: &sway_types::BaseIdent) -> TokenStream {
    let name = base_ident.as_str();
    let is_raw_ident = base_ident.is_raw_ident();
    quote!(sway_types::BaseIdent::new_with_raw(sway_types::Span::from_string(#name.into()), #is_raw_ident))
}

fn generate_option_pub_token_code(visibility: &Option<sway_ast::keywords::PubToken>) -> TokenStream {
    match visibility {
        Some(_) => quote!(Some(sway_ast::keywords::PubToken::new(sway_types::Span::dummy()))),
        None => quote!(None),
    }
}

fn generate_option_double_colon_token_code(token: &Option<sway_ast::keywords::DoubleColonToken>) -> TokenStream {
    match token {
        Some(_) => quote!(Some(sway_ast::keywords::DoubleColonToken::new(sway_types::Span::dummy()))),
        None => quote!(None),
    }
}

fn generate_item_submodule_code(item: &sway_ast::Submodule) -> TokenStream {
    let name = generate_base_ident_code(&item.name);
    let visibility = generate_option_pub_token_code(&item.visibility);
    
    quote!(sway_ast::Submodule {
        mod_token: sway_ast::keywords::ModToken::new(sway_types::Span::dummy()),
        name: #name,
        semicolon_token: sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()),
        visibility: #visibility,
    })
}

fn generate_item_use_code(item: &sway_ast::ItemUse) -> TokenStream {
    let visibility = generate_option_pub_token_code(&item.visibility);
    let root_import = generate_option_double_colon_token_code(&item.root_import);
    let tree = generate_use_tree_code(&item.tree);

    quote!(sway_ast::ItemUse {
        visibility: #visibility,
        use_token: sway_ast::keywords::UseToken::new(sway_types::Span::dummy()),
        root_import: #root_import,
        tree: #tree,
        semicolon_token: sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()),
    })
}

fn generate_item_struct_code(item: &sway_ast::ItemStruct) -> TokenStream {
    let visibility = generate_option_pub_token_code(&item.visibility);
    let name = generate_base_ident_code(&item.name);
    let generics = generate_option_generic_params_code(&item.generics);
    let where_clause_opt = generate_option_where_clause_code(&item.where_clause_opt);

    let value_separator_pairs = item.fields.inner.value_separator_pairs.iter().map(|x| {
        let attribute_list = x.0.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
        let value = generate_type_field_code(&x.0.value);

        quote!(
            (
                sway_ast::attribute::Annotated {
                    attribute_list: vec![#(#attribute_list),*],
                    value: #value,
                },
                sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
            )
        )
    }).collect::<Vec<_>>();

    let final_value_opt = match item.fields.inner.final_value_opt.as_ref() {
        Some(x) => {
            let attribute_list = x.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
            let value = generate_type_field_code(&x.value);
    
            quote!(Some(Box::new(sway_ast::attribute::Annotated {
                attribute_list: vec![#(#attribute_list),*],
                value: #value,
            })))
        }

        None => quote!(None),
    };

    quote!(sway_ast::ItemStruct {
        visibility: #visibility,
        struct_token: sway_ast::keywords::StructToken::new(sway_types::Span::dummy()),
        name: #name,
        generics: #generics,
        where_clause_opt: #where_clause_opt,
        fields: sway_ast::Braces {
            inner: sway_ast::Punctuated {
                value_separator_pairs: vec![#(#value_separator_pairs),*],
                final_value_opt: #final_value_opt,
            },
            span: sway_types::Span::dummy(),
        }
    })
}

fn generate_item_enum_code(item: &sway_ast::ItemEnum) -> TokenStream {
    let visibility = generate_option_pub_token_code(&item.visibility);
    let name = generate_base_ident_code(&item.name);
    let generics = generate_option_generic_params_code(&item.generics);
    let where_clause_opt = generate_option_where_clause_code(&item.where_clause_opt);

    let value_separator_pairs = item.fields.inner.value_separator_pairs.iter().map(|x| {
        let attribute_list = x.0.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
        let value = generate_type_field_code(&x.0.value);

        quote!(
            (
                sway_ast::attribute::Annotated {
                    attribute_list: vec![#(#attribute_list),*],
                    value: #value,
                },
                sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
            )
        )
    }).collect::<Vec<_>>();

    let final_value_opt = match item.fields.inner.final_value_opt.as_ref() {
        Some(x) => {
            let attribute_list = x.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
            let value = generate_type_field_code(&x.value);
    
            quote!(Some(Box::new(sway_ast::attribute::Annotated {
                attribute_list: vec![#(#attribute_list),*],
                value: #value,
            })))
        }

        None => quote!(None),
    };

    quote!(sway_ast::ItemEnum {
        visibility: #visibility,
        enum_token: sway_ast::keywords::EnumToken::new(sway_types::Span::dummy()),
        name: #name,
        generics: #generics,
        where_clause_opt: #where_clause_opt,
        fields: sway_ast::Braces {
            inner: sway_ast::Punctuated {
                value_separator_pairs: vec![#(#value_separator_pairs),*],
                final_value_opt: #final_value_opt,
            },
            span: sway_types::Span::dummy(),
        },
    })
}

fn generate_path_expr_segment_code(segment: &sway_ast::PathExprSegment) -> TokenStream {
    let name = generate_base_ident_code(&segment.name);

    let generics_opt = match &segment.generics_opt {
        Some(segment) => {
            let generic_args = generate_generic_args_code(&segment.1);
            quote!(Some((sway_ast::keywords::DoubleColonToken::new(sway_types::Span::dummy()), #generic_args)))
        }

        None => quote!(None),
    };

    quote!(sway_ast::PathExprSegment {
        name: #name,
        generics_opt: #generics_opt,
    })
}

fn generate_path_expr_code(path_expr: &sway_ast::PathExpr) -> TokenStream {
    let root_opt = match path_expr.root_opt.as_ref() {
        Some(root) => {
            let inner = match root.0.as_ref() {
                Some(root) => {
                    let inner = generate_qualified_path_root_code(&root.inner);

                    quote!(Some(sway_ast::AngleBrackets {
                        open_angle_bracket_token: sway_ast::keywords::OpenAngleBracketToken::new(sway_types::Span::dummy()),
                        inner: #inner,
                        close_angle_bracket_token: sway_ast::keywords::CloseAngleBracketToken::new(sway_types::Span::dummy()),
                    }))
                }

                None => quote!(None),
            };

            quote!(Some((#inner, sway_ast::keywords::DoubleColonToken::new(sway_types::Span::dummy()))))
        }

        None => quote!(None),
    };

    let prefix = generate_path_expr_segment_code(&path_expr.prefix);

    let suffix = path_expr.suffix.iter().map(|x| {
        let x = generate_path_expr_segment_code(&x.1);
        quote!(
            (
                sway_ast::keywords::DoubleColonToken::new(sway_types::Span::dummy()),
                #x
            )
        )
    }).collect::<Vec<_>>();

    let incomplete_suffix = path_expr.incomplete_suffix;
    
    quote!(sway_ast::PathExpr {
        root_opt: #root_opt,
        prefix: #prefix,
        suffix: vec![#(#suffix),*],
        incomplete_suffix: #incomplete_suffix,
    })
}

fn generate_pattern_struct_field_code(pattern: &sway_ast::PatternStructField) -> TokenStream {
    match pattern {
        sway_ast::PatternStructField::Rest { token: _ } => {
            quote!(sway_ast::PatternStructField::Rest {
                token: sway_ast::keywords::DoubleDotToken::new(sway_types::Span::dummy()),
            })
        }

        sway_ast::PatternStructField::Field { field_name, pattern_opt } => {
            let field_name = generate_base_ident_code(field_name);
            
            let pattern_opt = match pattern_opt.as_ref() {
                Some(pattern) => {
                    let pattern = generate_pattern_code(pattern.1.as_ref());
                    
                    quote!(
                        Some(
                            (
                                sway_ast::keywords::ColonToken::new(sway_types::Span::dummy()),
                                Box::new(#pattern)
                            )
                        )
                    )
                }

                None => quote!(None),
            };

            quote!(sway_ast::PatternStructField::Field {
                field_name: #field_name,
                pattern_opt: #pattern_opt,
            })
        }
    }
}

fn generate_pattern_code(pattern: &sway_ast::Pattern) -> TokenStream {
    match pattern {
        sway_ast::Pattern::Or { lhs, pipe_token: _, rhs } => {
            let lhs = generate_pattern_code(lhs.as_ref());
            let rhs = generate_pattern_code(rhs.as_ref());

            quote!(sway_ast::Pattern::Or {
                lhs: Box::new(#lhs),
                pipe_token: sway_ast::keywords::PipeToken::new(sway_types::Span::dummy()),
                rhs: Box::new(#rhs),
            })
        }

        sway_ast::Pattern::Wildcard { underscore_token: _ } => {
            quote!(sway_ast::Pattern::Wildcard {
                underscore_token: sway_ast::keywords::UnderscoreToken::new(sway_types::Span::dummy()),
            })
        }

        sway_ast::Pattern::AmbiguousSingleIdent(ident) => {
            let ident = generate_base_ident_code(ident);
            quote!(sway_ast::Pattern::AmbiguousSingleIdent(#ident))
        }

        sway_ast::Pattern::Var { reference, mutable, name } => {
            let reference = match reference.as_ref() {
                Some(_) => quote!(Some(sway_ast::keywords::RefToken::new(sway_types::Span::dummy()))),
                None => quote!(None),
            };

            let mutable = match mutable.as_ref() {
                Some(_) => quote!(Some(sway_ast::keywords::MutToken::new(sway_types::Span::dummy()))),
                None => quote!(None),
            };

            let name = generate_base_ident_code(name);

            quote!(sway_ast::Pattern::Var {
                reference: #reference,
                mutable: #mutable,
                name: #name,
            })
        }

        sway_ast::Pattern::Literal(literal) => {
            let literal = generate_literal_code(literal);
            quote!(sway_ast::Pattern::Literal(#literal))
        }

        sway_ast::Pattern::Constant(constant) => {
            let constant = generate_path_expr_code(constant);
            quote!(sway_ast::Pattern::Constant(#constant))
        }

        sway_ast::Pattern::Constructor { path, args } => {
            let path = generate_path_expr_code(path);
            
            let value_separator_pairs = args.inner.value_separator_pairs.iter().map(|x| {
                let x = generate_pattern_code(&x.0);
                quote!(
                    (
                        #x,
                        sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
                    )
                )
            }).collect::<Vec<_>>();

            let final_value_opt = match args.inner.final_value_opt.as_ref() {
                Some(x) => {
                    let x = generate_pattern_code(x.as_ref());
                    quote!(Some(Box::new(#x)))
                }

                None => quote!(None),
            };

            quote!(sway_ast::Pattern::Constructor {
                path: #path,
                args: sway_ast::Parens {
                    inner: sway_ast::Punctuated {
                        value_separator_pairs: vec![#(#value_separator_pairs),*],
                        final_value_opt: #final_value_opt,
                    },
                    span: sway_types::Span::dumy(),
                },
            })
        }

        sway_ast::Pattern::Struct { path, fields } => {
            let path = generate_path_expr_code(path);
            
            let value_separator_pairs = fields.inner.value_separator_pairs.iter().map(|x| {
                let x = generate_pattern_struct_field_code(&x.0);
                quote!(
                    (
                        #x,
                        sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
                    )
                )
            }).collect::<Vec<_>>();

            let final_value_opt = match fields.inner.final_value_opt.as_ref() {
                Some(x) => {
                    let x = generate_pattern_struct_field_code(x.as_ref());
                    quote!(Some(Box::new(#x)))
                }

                None => quote!(None),
            };

            quote!(sway_ast::Pattern::Struct {
                path: #path,
                fields: sway_ast::Braces {
                    inner: sway_ast::Punctuated {
                        value_separator_pairs: vec![#(#value_separator_pairs),*],
                        final_value_opt: #final_value_opt,
                    },
                    span: sway_types::Span::dummy(),
                },
            })
        }

        sway_ast::Pattern::Tuple(tuple) => {
            let value_separator_pairs = tuple.inner.value_separator_pairs.iter().map(|x| {
                let x = generate_pattern_code(&x.0);
                quote!(
                    (
                        #x,
                        sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
                    )
                )
            }).collect::<Vec<_>>();

            let final_value_opt = match tuple.inner.final_value_opt.as_ref() {
                Some(x) => {
                    let x = generate_pattern_code(x.as_ref());
                    quote!(Some(Box::new(#x)))
                }

                None => quote!(None),
            };

            quote!(sway_ast::Pattern::Tuple(sway_ast::Parens {
                inner: sway_ast::Punctuated {
                    value_separator_pairs: vec![#(#value_separator_pairs),*],
                    final_value_opt: #final_value_opt,
                },
                span: sway_types::Span::dummy(),
            }))
        }

        sway_ast::Pattern::Error(_, _) => {
            panic!("An error occurred while parsing Sway AST")
        }
    }
}

fn generate_fn_arg_code(arg: &sway_ast::FnArg) -> TokenStream {
    let pattern = generate_pattern_code(&arg.pattern);
    let ty = generate_ty_code(&arg.ty);

    quote!(sway_ast::FnArg {
        pattern: #pattern,
        colon_token: sway_ast::keywords::ColonToken::new(sway_types::Span::dummy()),
        ty: #ty,
    })
}

fn generate_fn_args_code(args: &sway_ast::FnArgs) -> TokenStream {
    match args {
        sway_ast::FnArgs::Static(args) => {
            let value_separator_pairs = args.value_separator_pairs.iter().map(|x| {
                let x = generate_fn_arg_code(&x.0);
                quote!(
                    (
                        #x,
                        sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
                    )
                )
            }).collect::<Vec<_>>();

            let final_value_opt = match args.final_value_opt.as_ref() {
                Some(x) => {
                    let x = generate_fn_arg_code(x.as_ref());
                    quote!(Some(Box::new(#x)))
                }

                None => quote!(None)
            };

            quote!(sway_ast::FnArgs::Static(sway_ast::Punctuated {
                value_separator_pairs: vec![#(#value_separator_pairs),*],
                final_value_opt: #final_value_opt,
            }))
        }

        sway_ast::FnArgs::NonStatic { self_token: _, ref_self, mutable_self, args_opt } => {
            let ref_self = match ref_self.as_ref() {
                Some(_) => quote!(Some(sway_ast::keywords::RefToken::new(sway_types::Span::dummy()))),
                None => quote!(None),
            };

            let mutable_self = match mutable_self.as_ref() {
                Some(_) => quote!(Some(sway_ast::keywords::MutToken::new(sway_types::Span::dummy()))),
                None => quote!(None),
            };

            // Option<(CommaToken, Punctuated<FnArg, CommaToken>)>
            let args_opt = match args_opt.as_ref() {
                Some(args) => {
                    let value_separator_pairs = args.1.value_separator_pairs.iter().map(|x| {
                        let x = generate_fn_arg_code(&x.0);
                        quote!(
                            (
                                #x,
                                sway_ast::keywords::CommaToken::new(sway_types::Span::dummy()),
                            )
                        )
                    }).collect::<Vec<_>>();

                    let final_value_opt = match args.1.final_value_opt.as_ref() {
                        Some(x) => {
                            let x = generate_fn_arg_code(x);
                            quote!(Some(Box::new(#x)))
                        }

                        None => quote!(None),
                    };

                    quote!(
                        Some(
                            (
                                sway_ast::keywords::CommaToken::new(sway_types::Span::dummy()),
                                sway_ast::Punctuated {
                                    value_separator_pairs: vec![#(#value_separator_pairs),*],
                                    final_value_opt: #final_value_opt,
                                }
                            )
                        )
                    )
                }

                None => quote!(None),
            };

            quote!(sway_ast::FnArgs::NonStatic {
                self_token: sway_ast::keywords::SelfToken::new(sway_types::Span::dummy()),
                ref_self: #ref_self,
                mutable_self: #mutable_self,
                args_opt: #args_opt,
            })
        }
    }
}

fn generate_fn_signature_code(fn_signature: &sway_ast::FnSignature) -> TokenStream {
    let visibility = generate_option_pub_token_code(&fn_signature.visibility);
    let name = generate_base_ident_code(&fn_signature.name);
    let generics = generate_option_generic_params_code(&fn_signature.generics);
    let arguments = generate_fn_args_code(&fn_signature.arguments.inner);
    
    let return_type_opt = match fn_signature.return_type_opt.as_ref() {
        Some(return_type) => {
            let ty = generate_ty_code(&return_type.1);
            
            quote!(
                Some(
                    (
                        sway_ast::keywords::RightArrowToken::new(sway_types::Span::dummy()),
                        #ty
                    )
                )
            )
        }

        None => quote!(None),
    };
    
    let where_clause_opt = generate_option_where_clause_code(&fn_signature.where_clause_opt);

    quote!(sway_ast::FnSignature {
        visibility: #visibility,
        fn_token: sway_ast::keywords::FnToken::new(sway_types::Span::dummy()),
        name: #name,
        generics: #generics,
        arguments: sway_ast::Parens {
            inner: #arguments,
            span: sway_types::Span::dummy(),
        },
        return_type_opt: #return_type_opt,
        where_clause_opt: #where_clause_opt,
    })
}

fn generate_item_fn_code(item: &sway_ast::ItemFn) -> TokenStream {
    let fn_signature = generate_fn_signature_code(&item.fn_signature);

    quote!(sway_ast::ItemFn {
        fn_signature: #fn_signature,
        body: sway_ast::Braces {
            inner: sway_ast::CodeBlockContents {
                statements: vec![
                    /* NOTE: We are not going to bother with converting function bodies, we just need them for their signatures. */
                ],
                final_expr_opt: None,
                span: sway_types::Span::dummy(),
            },
            span: sway_types::Span::dummy(),
        },
    })
}

fn generate_item_const_code(item: &sway_ast::ItemConst) -> TokenStream {
    let visibility = generate_option_pub_token_code(&item.visibility);
    let name = generate_base_ident_code(&item.name);

    let ty_opt = match item.ty_opt.as_ref() {
        Some(ty) => {
            let ty = generate_ty_code(&ty.1);
            quote!(
                Some(
                    (
                        sway_ast::keywords::ColonToken::new(sway_types::Span::dummy()),
                        #ty
                    )
                )
            )
        }

        None => quote!(None),
    };

    let eq_token_opt = match item.eq_token_opt.as_ref() {
        Some(_) => quote!(Some(sway_ast::keywords::EqToken::new(sway_types::Span::dummy()))),
        None => quote!(None),
    };

    let expr_opt = match item.expr_opt.as_ref() {
        Some(expr) => {
            let expr = generate_expr_code(expr);
            quote!(Some(#expr))
        }

        None => quote!(None),
    };

    quote!(sway_ast::ItemConst {
        visibility: #visibility,
        const_token: sway_ast::keywords::ConstToken::new(sway_types::Span::dummy()),
        name: #name,
        ty_opt: #ty_opt,
        eq_token_opt: #eq_token_opt,
        expr_opt: #expr_opt,
        semicolon_token: sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()),
    })
}

fn generate_trait_type_code(trait_type: &sway_ast::TraitType) -> TokenStream {
    let name = generate_base_ident_code(&trait_type.name);

    let eq_token_opt = match trait_type.eq_token_opt.as_ref() {
        Some(_) => quote!(Some(sway_ast::keywords::EqToken::new(sway_types::Span::dummy()))),
        None => quote!(None),
    };

    let ty_opt = match trait_type.ty_opt.as_ref() {
        Some(ty) => {
            let ty = generate_ty_code(ty);
            quote!(Some(#ty))
        }

        None => quote!(None)
    };

    quote!(sway_ast::TraitType {
        name: #name,
        type_token: sway_ast::keywords::TypeToken::new(sway_types::Span::dummy()),
        eq_token_opt: #eq_token_opt,
        ty_opt: #ty_opt,
        semicolon_token: sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()),
    })
}

fn generate_item_trait_item_code(item: &sway_ast::ItemTraitItem) -> TokenStream {
    match item {
        sway_ast::ItemTraitItem::Fn(fn_signature, semicolon_token_opt) => {
            let fn_signature = generate_fn_signature_code(fn_signature);

            let semicolon_token_opt = match semicolon_token_opt.as_ref() {
                Some(_) => quote!(Some(sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()))),
                None => quote!(None),
            };

            quote!(sway_ast::ItemTraitItem::Fn(#fn_signature, #semicolon_token_opt))
        }

        sway_ast::ItemTraitItem::Const(item_const, semicolon_token_opt) => {
            let item_const = generate_item_const_code(item_const);

            let semicolon_token_opt = match semicolon_token_opt.as_ref() {
                Some(_) => quote!(Some(sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()))),
                None => quote!(None),
            };

            quote!(sway_ast::ItemTraitItem::Const(#item_const, #semicolon_token_opt))
        }

        sway_ast::ItemTraitItem::Type(item_type, semicolon_token_opt) => {
            let item_type = generate_trait_type_code(item_type);

            let semicolon_token_opt = match semicolon_token_opt.as_ref() {
                Some(_) => quote!(Some(sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()))),
                None => quote!(None),
            };

            quote!(sway_ast::ItemTraitItem::Type(#item_type, #semicolon_token_opt))
        }
        
        sway_ast::ItemTraitItem::Error(_, _) => {
            panic!("An error occurred while parsing Sway AST")
        }
    }
}

fn generate_item_trait_code(item: &sway_ast::ItemTrait) -> TokenStream {
    let visibility = generate_option_pub_token_code(&item.visibility);
    let name = generate_base_ident_code(&item.name);
    let generics = generate_option_generic_params_code(&item.generics);
    let where_clause_opt = generate_option_where_clause_code(&item.where_clause_opt);
    
    let super_traits = match item.super_traits.as_ref() {
        Some(super_traits) => {
            let x = generate_traits_code(&super_traits.1);
            quote!(
                Some(
                    (
                        sway_ast::keywords::ColonToken::new(sway_types::Span::dummy()),
                        #x
                    )
                )
            )
        }

        None => quote!(None),
    };

    let trait_items = item.trait_items.inner.iter().map(|x| {
        let attribute_list = x.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
        let value = generate_item_trait_item_code(&x.value);

        quote!(sway_ast::attribute::Annotated {
            attribute_list: vec![#(#attribute_list),*],
            value: #value,
        })
    }).collect::<Vec<_>>();

    let trait_defs_opt = match item.trait_defs_opt.as_ref() {
        Some(trait_defs) => {
            let trait_defs = trait_defs.inner.iter().map(|x| {
                let attribute_list = x.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
                let value = generate_item_fn_code(&x.value);

                quote!(sway_ast::attribute::Annotated {
                    attribute_list: vec![#(#attribute_list),*],
                    value: #value,
                })
            }).collect::<Vec<_>>();

            quote!(Some(sway_ast::Braces {
                inner: vec![#(#trait_defs),*],
                span: sway_types::Span::dummy(),
            }))
        }

        None => quote!(None),
    };

    quote!(sway_ast::ItemTrait {
        visibility: #visibility,
        trait_token: sway_ast::keywords::TraitToken::new(sway_types::Span::dummy()),
        name: #name,
        generics: #generics,
        where_clause_opt: #where_clause_opt,
        super_traits: #super_traits,
        trait_items: sway_ast::Braces {
            inner: vec![#(#trait_items),*],
            span: sway_types::Span::dummy(),
        },
        trait_defs_opt: #trait_defs_opt,
    })
}

fn generate_item_impl_item_code(item: &sway_ast::ItemImplItem) -> TokenStream {
    match item {
        sway_ast::ItemImplItem::Fn(item_fn) => {
            let item_fn = generate_item_fn_code(item_fn);
            quote!(sway_ast::ItemImplItem::Fn(#item_fn))
        }

        sway_ast::ItemImplItem::Const(item_const) => {
            let item_const = generate_item_const_code(item_const);
            quote!(sway_ast::ItemImplItem::Const(#item_const))
        }

        sway_ast::ItemImplItem::Type(trait_type) => {
            let trait_type = generate_trait_type_code(trait_type);
            quote!(sway_ast::ItemImplItem::Type(#trait_type))
        }
    }
}

fn generate_item_impl_code(item: &sway_ast::ItemImpl) -> TokenStream {
    let generic_params_opt = generate_option_generic_params_code(&item.generic_params_opt);

    let trait_opt = match item.trait_opt.as_ref() {
        Some(for_trait) => {
            let path_type = generate_path_type_code(&for_trait.0);
            quote!(
                Some(
                    (
                        #path_type,
                        sway_ast::keywords::ForToken::new(sway_types::Span::dummy())
                    )
                )
            )
        }

        None => quote!(None),
    };

    let ty = generate_ty_code(&item.ty);
    let where_clause_opt = generate_option_where_clause_code(&item.where_clause_opt);

    let contents = item.contents.inner.iter().map(|x| {
        let attribute_list = x.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
        let value = generate_item_impl_item_code(&x.value);

        quote!(sway_ast::attribute::Annotated {
            attribute_list: vec![#(#attribute_list),*],
            value: #value,
        })
    }).collect::<Vec<_>>();

    quote!(sway_ast::ItemImpl {
        impl_token: sway_ast::keywords::ImplToken::new(sway_types::Span::dummy()),
        generic_params_opt: #generic_params_opt,
        trait_opt: #trait_opt,
        ty: #ty,
        where_clause_opt: #where_clause_opt,
        contents: sway_ast::Braces {
            inner: vec![#(#contents),*],
            span: sway_types::Span::dummy(),
        },
    })
}

fn generate_item_abi_code(item: &sway_ast::ItemAbi) -> TokenStream {
    let name = generate_base_ident_code(&item.name);
    
    let super_traits = match item.super_traits.as_ref() {
        Some(super_traits) => {
            let traits = generate_traits_code(&super_traits.1);
            quote!(
                Some(
                    (
                        sway_ast::keywords::ColonToken::new(sway_types::Span::dummy()),
                        #traits
                    )
                )
            )
        }

        None => quote!(None),
    };

    let abi_items = item.abi_items.inner.iter().map(|x| {
        let attribute_list = x.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
        let value = generate_item_trait_item_code(&x.value);

        quote!(sway_ast::attribute::Annotated {
            attribute_list: vec![#(#attribute_list),*],
            value: #value,
        })
    }).collect::<Vec<_>>();

    let abi_defs_opt = match item.abi_defs_opt.as_ref() {
        Some(abi_defs) => {
            let inner = abi_defs.inner.iter().map(|x| {
                let attribute_list = x.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
                let value = generate_item_fn_code(&x.value);

                quote!(sway_ast::attribute::Annotated {
                    attribute_list: vec![#(#attribute_list),*],
                    value: #value,
                })
            }).collect::<Vec<_>>();

            quote!(sway_ast::Braces {
                inner: vec![#(#inner),*],
                span: sway_types::Span::dummy(),
            })
        }

        None => quote!(None),
    };

    quote!(sway_ast::ItemAbi {
        abi_token: sway_ast::keywords::AbiToken::new(sway_types::Span::dummy()),
        name: #name,
        super_traits: #super_traits,
        abi_items: sway_ast::Braces {
            inner: vec![#(#abi_items),*],
            span: sway_types::Span::dummy(),
        },
        abi_defs_opt: #abi_defs_opt,
    })
}

fn generate_storage_field_code(field: &sway_ast::StorageField) -> TokenStream {
    let name = generate_base_ident_code(&field.name);
    let ty = generate_ty_code(&field.ty);
    let initializer = generate_expr_code(&field.initializer);

    quote!(sway_ast::StorageField {
        name: #name,
        colon_token: sway_ast::keywords::ColonToken::new(sway_types::Span::dummy()),
        ty: #ty,
        eq_token: sway_ast::keywords::EqToken::new(sway_types::Span::dummy()),
        initializer: #initializer,
    })
}

fn generate_item_storage_code(item: &sway_ast::ItemStorage) -> TokenStream {
    let value_separator_pairs = item.fields.inner.value_separator_pairs.iter().map(|x| {
        let attribute_list = x.0.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
        let value = generate_storage_field_code(&x.0.value);

        quote!(
            (
                sway_ast::attribute::Annotated {
                    attribute_list: vec![#(#attribute_list),*],
                    value: #value,
                },
                sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
            )
        )
    }).collect::<Vec<_>>();

    let final_value_opt = match item.fields.inner.final_value_opt.as_ref() {
        Some(x) => {
            let attribute_list = x.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
            let value = generate_storage_field_code(&x.value);
    
            quote!(Some(Box::new(sway_ast::attribute::Annotated {
                attribute_list: vec![#(#attribute_list),*],
                value: #value,
            })))
        }

        None => quote!(None),
    };

    quote!(sway_ast::ItemStorage {
        storage_token: sway_ast::keywords::StorageToken::new(sway_types::Span::dummy()),
        fields: sway_ast::Braces {
            inner: sway_ast::Punctuated {
                value_separator_pairs: vec![#(#value_separator_pairs),*],
                final_value_opt: #final_value_opt,
            },
            span: sway_types::Span::dummy(),
        },
    })
}

fn generate_configurable_field_code(field: &sway_ast::ConfigurableField) -> TokenStream {
    let name = generate_base_ident_code(&field.name);
    let ty = generate_ty_code(&field.ty);
    let initializer = generate_expr_code(&field.initializer);

    quote!(sway_ast::ConfigurableField {
        name: #name,
        colon_token: sway_ast::keywords::ColonToken::new(sway_types::Span::dummy()),
        ty: #ty,
        eq_token: sway_ast::keywords::EqToken::new(sway_types::Span::dummy()),
        initializer: #initializer,
    })
}

fn generate_item_configurable_code(item: &sway_ast::ItemConfigurable) -> TokenStream {
    let value_separator_pairs = item.fields.inner.value_separator_pairs.iter().map(|x| {
        let attribute_list = x.0.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
        let value = generate_configurable_field_code(&x.0.value);

        quote!(
            (
                sway_ast::attribute::Annotated {
                    attribute_list: vec![#(#attribute_list),*],
                    value: #value,
                },
                sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
            )
        )
    }).collect::<Vec<_>>();

    let final_value_opt = match item.fields.inner.final_value_opt.as_ref() {
        Some(x) => {
            let attribute_list = x.attribute_list.iter().map(|x| generate_attribute_decl_code(x)).collect::<Vec<_>>();
            let value = generate_configurable_field_code(&x.value);
    
            quote!(Some(Box::new(sway_ast::attribute::Annotated {
                attribute_list: vec![#(#attribute_list),*],
                value: #value,
            })))
        }

        None => quote!(None),
    };

    quote!(sway_ast::ItemConfigurable {
        configurable_token: sway_ast::keywords::ConfigurableToken::new(sway_types::Span::dummy()),
        fields: sway_ast::Braces {
            inner: sway_ast::Punctuated {
                value_separator_pairs: vec![#(#value_separator_pairs),*],
                final_value_opt: #final_value_opt,
            },
            span: sway_types::Span::dummy(),
        },
    })
}

fn generate_item_type_alias_code(item: &sway_ast::ItemTypeAlias) -> TokenStream {
    let visibility = generate_option_pub_token_code(&item.visibility);
    let name = generate_base_ident_code(&item.name);
    let ty = generate_ty_code(&item.ty);

    quote!(sway_ast::ItemTypeAlias {
        visibility: #visibility,
        name: #name,
        type_token: sway_ast::keywords::TypeToken::new(sway_types::Span::dummy()),
        eq_token: sway_ast::keywords::EqToken::new(sway_types::Span::dummy()),
        ty: #ty,
        semicolon_token: sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()),
    })
}

fn generate_use_tree_code(tree: &sway_ast::UseTree) -> TokenStream {
    match tree {
        sway_ast::UseTree::Group { imports } => {
            let mut value_separator_pairs = vec![];

            for value_separator_pair in imports.inner.value_separator_pairs.iter() {
                let tree = generate_use_tree_code(&value_separator_pair.0);

                value_separator_pairs.push(quote!(
                    (
                        #tree,
                        sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
                    )
                ));
            }

            let final_value_opt = match imports.inner.final_value_opt.as_ref() {
                Some(tree) => {
                    let tree = generate_use_tree_code(tree);
                    quote!(Some(Box::new(#tree)))
                }

                None => quote!(None),
            };
            
            quote!(sway_ast::UseTree::Group {
                imports: sway_ast::Braces {
                    inner: sway_ast::Punctuated {
                        value_separator_pairs: vec![#(#value_separator_pairs),*],
                        final_value_opt: #final_value_opt,
                    },
                    span: sway_types::Span::dummy(),
                },
            })
        }

        sway_ast::UseTree::Name { name } => {
            let name = generate_base_ident_code(name);

            quote!(sway_ast::UseTree::Name { name: #name })
        }

        sway_ast::UseTree::Rename { name, as_token: _, alias } => {
            let name = generate_base_ident_code(name);
            let alias = generate_base_ident_code(alias);

            quote!(sway_ast::UseTree::Rename {
                name: #name,
                as_token: sway_ast::keywords::AsToken::new(sway_types::Span::dummy()),
                alias: #alias,
            })
        }

        sway_ast::UseTree::Glob { star_token: _ } => {
            quote!(sway_ast::UseTree::Glob {
                star_token: sway_ast::keywords::StarToken::new(sway_types::Span::dummy()),
            })
        }

        sway_ast::UseTree::Path { prefix, double_colon_token: _, suffix } => {
            let prefix = generate_base_ident_code(prefix);
            let suffix = generate_use_tree_code(suffix);

            quote!(sway_ast::UseTree::Path {
                prefix: #prefix,
                double_colon_token: sway_ast::keywords::DoubleColonToken::new(sway_types::Span::dummy()),
                suffix: Box::new(#suffix),
            })
        }

        sway_ast::UseTree::Error { spans: _ } => {
            panic!("An error occurred while parsing Sway AST")
        }
    }
}

fn generate_option_generic_params_code(generics: &Option<sway_ast::GenericParams>) -> TokenStream {
    match generics {
        Some(generics) => {
            let value_separator_pairs = generics.parameters.inner.value_separator_pairs.iter().map(|x| {
                let ident = generate_base_ident_code(&x.0);

                quote!(
                    (
                        #ident,
                        sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
                    )
                )
            }).collect::<Vec<_>>();

            let final_value_opt = match generics.parameters.inner.final_value_opt.as_ref() {
                Some(ident) => {
                    let ident = generate_base_ident_code(ident);
                    quote!(Some(Box::new(#ident)))
                }

                None => quote!(None),
            };

            quote!(Some(sway_ast::GenericParams {
                parameters: sway_ast::AngleBrackets {
                    open_angle_bracket_token: sway_ast::keywords::OpenAngleBracketToken::new(sway_types::Span::dummy()),
                    inner: sway_ast::Punctuated {
                        value_separator_pairs: vec![#(#value_separator_pairs),*],
                        final_value_opt: #final_value_opt,
                    },
                    close_angle_bracket_token: sway_ast::keywords::CloseAngleBracketToken::new(sway_types::Span::dummy()),
                },
            }))
        }

        None => quote!(None),
    }
}

fn generate_ty_code(ty: &sway_ast::Ty) -> TokenStream {
    match ty {
        sway_ast::Ty::Path(path_type) => {
            let path_type = generate_path_type_code(path_type);
            quote!(sway_ast::Ty::Path(#path_type))
        }

        sway_ast::Ty::Tuple(tuple) => {
            let inner = match &tuple.inner {
                sway_ast::ty::TyTupleDescriptor::Nil => quote!(sway_ast::ty::TyTupleDescriptor::Nil),
                
                sway_ast::ty::TyTupleDescriptor::Cons { head, comma_token: _, tail } => {
                    let head = generate_ty_code(head.as_ref());
                    
                    let value_separator_pairs = tail.value_separator_pairs.iter().map(|x| {
                        let x = generate_ty_code(&x.0);
                        quote!(
                            (
                                #x,
                                sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
                            )
                        )
                    }).collect::<Vec<_>>();

                    let final_value_opt = match tail.final_value_opt.as_ref() {
                        Some(x) => {
                            let x = generate_ty_code(x.as_ref());
                            quote!(Some(Box::new(#x)))
                        }

                        None => quote!(None),
                    };

                    let tail = quote!(sway_ast::Punctuated {
                        value_separator_pairs: vec![#(#value_separator_pairs),*],
                        final_value_opt: #final_value_opt,
                    });

                    quote!(sway_ast::ty::TyTupleDescriptor::Cons {
                        head: Box::new(#head),
                        comma_token: sway_ast::keywords::CommaToken::new(sway_types::Span::dummy()),
                        tail: #tail,
                    })
                }
            };

            quote!(sway_ast::Ty::Tuple(sway_ast::Parens {
                inner: #inner,
                span: sway_types::Span::dummy(),
            }))
        }

        sway_ast::Ty::Array(array) => {
            let ty = generate_ty_code(&array.inner.ty);
            let length = generate_expr_code(array.inner.length.as_ref());

            quote!(sway_ast::Ty::Array(sway_ast::brackets::SquareBrackets {
                inner: sway_ast::ty::TyArrayDescriptor {
                    ty: Box::new(#ty),
                    semicolon_token: sway_ast::keywords::SemicolonToken::new(sway_types::Span::dummy()),
                    length: Box::new(#length),
                },
                span: sway_types::Span::dummy(),
            }))
        }

        sway_ast::Ty::StringSlice(_) => quote!(sway_ast::Ty::StringSlice(sway_ast::keywords::StrToken::new(sway_types::Span::dummy()))),

        sway_ast::Ty::StringArray { str_token: _, length } => {
            let length = generate_expr_code(length.inner.as_ref());

            let length = quote!(sway_ast::brackets::SquareBrackets {
                inner: Box::new(#length),
                span: sway_types::Span::dummy(),
            });

            quote!(sway_ast::Ty::StringArray {
                str_token: sway_ast::keywords::StrToken::new(sway_types::Span::dummy()),
                length: #length,
            })
        }

        sway_ast::Ty::Infer { underscore_token: _ } => {
            quote!(sway_ast::Ty::Infer {
                underscore_token: sway_ast::keywords::UnderscoreToken::new(sway_types::Span::dummy()),
            })
        }

        sway_ast::Ty::Ptr { ptr_token: _, ty } => {
            let inner = generate_ty_code(ty.inner.as_ref());
            
            quote!(sway_ast::Ty::Ptr {
                ptr_token: sway_ast::keywords::PtrToken::new(sway_types::Span::dummy()),
                ty: sway_ast::brackets::SquareBrackets {
                    inner: Box::new(#inner),
                    span: sway_types::Span::dummy(),
                },
            })
        }

        sway_ast::Ty::Slice { slice_token: _, ty } => {
            let inner = generate_ty_code(ty.inner.as_ref());

            quote!(sway_ast::Ty::Slice {
                slice_token: sway_ast::keywords::SliceToken::new(sway_types::Span::dummy()),
                ty: sway_ast::brackets::SquareBrackets {
                    inner: Box::new(#inner),
                    span: sway_types::Span::dummy(),
                },
            })
        }

        sway_ast::Ty::Ref { ampersand_token: _, mut_token, ty } => {
            let mut_token = match mut_token.as_ref() {
                Some(_) => quote!(Some(sway_ast::keywords::MutToken::new(sway_types::Span::dummy()))),
                None => quote!(None),
            };

            let ty = generate_ty_code(ty.as_ref());

            quote!(sway_ast::Ty::Ref {
                ampersand_token: sway_ast::keywords::AmpersandToken::new(sway_types::Span::dummy()),
                mut_token: #mut_token,
                ty: Box::new(#ty),
            })
        }
        
        sway_ast::Ty::Never { bang_token: _ } => {
            quote!(sway_ast::Ty::Never {
                bang_token: sway_ast::keywords::BangToken::new(sway_types::Span::dummy()),
            })
        }
    }
}

fn generate_qualified_path_root_code(root: &sway_ast::QualifiedPathRoot) -> TokenStream {
    let ty = generate_ty_code(&root.ty);

    let as_trait = match root.as_trait.as_ref() {
        Some(as_trait) => {
            let path_type = generate_path_type_code(as_trait.1.as_ref());

            quote!(
                (
                    sway_ast::keywords::AsToken::new(sway_types::Span::dummy()),
                    Box::new(#path_type)
                )
            )
        }
        
        None => quote!(None),
    };

    quote!(sway_ast::QualifiedPathRoot {
        ty: #ty,
        as_trait: #as_trait,
    })
}

fn generate_generic_args_code(generic_args: &sway_ast::GenericArgs) -> TokenStream {
    let value_separator_pairs = generic_args.parameters.inner.value_separator_pairs.iter().map(|x| {
        let ty = generate_ty_code(&x.0);

        quote!(
            (
                #ty,
                sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
            )
        )
    }).collect::<Vec<_>>();

    let final_value_opt = match generic_args.parameters.inner.final_value_opt.as_ref() {
        Some(ty) => {
            let ty = generate_ty_code(ty.as_ref());
            quote!(Some(Box::new(#ty)))
        }

        None => quote!(None)
    };

    quote!(sway_ast::GenericArgs {
        parameters: sway_ast::brackets::AngleBrackets {
            open_angle_bracket_token: sway_ast::keywords::OpenAngleBracketToken::new(sway_types::Span::dummy()),
            inner: sway_ast::Punctuated {
                value_separator_pairs: vec![#(#value_separator_pairs),*],
                final_value_opt: #final_value_opt,
            },
            close_angle_bracket_token: sway_ast::keywords::CloseAngleBracketToken::new(sway_types::Span::dummy()),
        },
    })
}

fn generate_path_type_segment_code(segment: &sway_ast::PathTypeSegment) -> TokenStream {
    let name = generate_base_ident_code(&segment.name);

    let generics_opt = match &segment.generics_opt {
        Some(segment) => {
            let token = match segment.0.as_ref() {
                Some(_) => quote!(Some(sway_ast::keywords::DoubleColonToken::new(sway_types::Span::dummy()))),
                None => quote!(None),
            };

            let generic_args = generate_generic_args_code(&segment.1);

            quote!(Some((#token, #generic_args)))
        }

        None => quote!(None),
    };

    quote!(sway_ast::PathTypeSegment {
        name: #name,
        generics_opt: #generics_opt,
    })
}

fn generate_path_type_code(path_type: &sway_ast::PathType) -> TokenStream {
    let root_opt = match path_type.root_opt.as_ref() {
        Some(root) => {
            let inner = match root.0.as_ref() {
                Some(root) => {
                    let inner = generate_qualified_path_root_code(&root.inner);

                    quote!(Some(sway_ast::AngleBrackets {
                        open_angle_bracket_token: sway_ast::keywords::OpenAngleBracketToken::new(sway_types::Span::dummy()),
                        inner: #inner,
                        close_angle_bracket_token: sway_ast::keywords::CloseAngleBracketToken::new(sway_types::Span::dummy()),
                    }))
                }

                None => quote!(None),
            };

            quote!(Some((#inner, sway_ast::keywords::DoubleColonToken::new(sway_types::Span::dummy()))))
        }

        None => quote!(None),
    };

    let prefix = generate_path_type_segment_code(&path_type.prefix);

    let suffix = path_type.suffix.iter().map(|x| {
        let x = generate_path_type_segment_code(&x.1);
        quote!(
            (
                sway_ast::keywords::DoubleColonToken::new(sway_types::Span::dummy()),
                #x
            )
        )
    }).collect::<Vec<_>>();

    quote!(sway_ast::PathType {
        root_opt: #root_opt,
        prefix: #prefix,
        suffix: vec![#(#suffix),*],
    })
}

fn generate_traits_code(traits: &sway_ast::Traits) -> TokenStream {
    let prefix = generate_path_type_code(&traits.prefix);
    let suffixes = traits.suffixes.iter().map(|x| {
        let x = generate_path_type_code(&x.1);
        quote!(
            (
                sway_ast::keywords::AddToken::new(sway_types::Span::dummy()),
                #x
            )
        )
    }).collect::<Vec<_>>();

    quote!(sway_ast::Traits {
        prefix: #prefix,
        suffixes: vec![#(#suffixes),*],
    })
}

fn generate_where_bound_code(where_bound: &sway_ast::WhereBound) -> TokenStream {
    let ty_name = generate_base_ident_code(&where_bound.ty_name);
    let bounds = generate_traits_code(&where_bound.bounds);

    quote!(sway_ast::WhereBound {
        ty_name: #ty_name,
        colon_token: sway_ast::keywords::ColonToken::new(sway_types::Span::dummy()),
        bounds: #bounds,
    })
}

fn generate_option_where_clause_code(where_clause_opt: &Option<sway_ast::WhereClause>) -> TokenStream {
    match where_clause_opt {
        Some(where_clause) => {
            let value_separator_pairs = where_clause.bounds.value_separator_pairs.iter().map(|x| {
                let where_bound = generate_where_bound_code(&x.0);
                quote!(
                    (
                        #where_bound,
                        sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
                    )
                )
            }).collect::<Vec<_>>();
            
            let final_value_opt = match where_clause.bounds.final_value_opt.as_ref() {
                Some(where_bound) => {
                    let where_bound = generate_where_bound_code(where_bound);
                    quote!(Some(Box::new(#where_bound)))
                }

                None => quote!(None),
            };

            quote!(Some(sway_ast::WhereClause {
                where_token: sway_ast::keywords::WhereToken::new(sway_types::Span::dummy()),
                bounds: sway_ast::Punctuated {
                    value_separator_pairs: vec![#(#value_separator_pairs),*],
                    final_value_opt: #final_value_opt,
                },
            }))
        }

        None => quote!(None)
    }
}

fn generate_type_field_code(type_field: &sway_ast::TypeField) -> TokenStream {
    let visibility = generate_option_pub_token_code(&type_field.visibility);
    let name = generate_base_ident_code(&type_field.name);
    let ty = generate_ty_code(&type_field.ty);

    quote!(sway_ast::TypeField {
        visibility: #visibility,
        name: #name,
        colon_token: sway_ast::keywords::ColonToken::new(sway_types::Span::dummy()),
        ty: #ty,
    })
}

fn generate_attribute_decl_code(
    attribute_decl: &sway_ast::attribute::AttributeDecl,
) -> TokenStream {
    let hash_kind = generate_attribute_hash_kind_code(&attribute_decl.hash_kind);

    let mut value_separator_pairs: Vec<TokenStream> = vec![];

    for value_separator_pair in attribute_decl.attribute.inner.value_separator_pairs.iter() {
        let attribute = generate_attribute_code(&value_separator_pair.0);
        value_separator_pairs.push(quote!(
            (
                #attribute,
                sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
            )
        ));
    }

    let final_value_opt = match attribute_decl.attribute.inner.final_value_opt.as_ref() {
        Some(arg) => {
            let attribute = generate_attribute_code(arg);
            quote!(Some(Box::new(#attribute)))
        }
        None => quote!(None),
    };

    quote!(sway_ast::attribute::AttributeDecl {
        hash_kind: #hash_kind,
        attribute: sway_ast::brackets::SquareBrackets {
            inner: sway_ast::Punctuated {
                value_separator_pairs: vec![#(#value_separator_pairs),*],
                final_value_opt: #final_value_opt,
            },
            span: sway_types::Span::dummy(),
        },
    })
}

fn generate_attribute_hash_kind_code(hash_kind: &sway_ast::attribute::AttributeHashKind) -> TokenStream {
    let (hash_kind, token_kind) = match hash_kind {
        sway_ast::attribute::AttributeHashKind::Inner(_) => (quote!(Inner), quote!(HashBangToken)),
        sway_ast::attribute::AttributeHashKind::Outer(_) => (quote!(Outer), quote!(HashToken)),
    };

    quote!(sway_ast::attribute::AttributeHashKind::#hash_kind(
        sway_ast::keywords::#token_kind::new(sway_types::Span::dummy())
    ))
}

fn generate_attribute_code(arg: &sway_ast::attribute::Attribute) -> TokenStream {
    let name = generate_base_ident_code(&arg.name);

    let args = match arg.args.as_ref() {
        Some(args) => {
            let mut value_separated_pairs = vec![];

            for value_separated_pair in args.inner.value_separator_pairs.iter() {
                let arg = generate_attribute_arg_code(&value_separated_pair.0);

                value_separated_pairs.push(quote!(
                    (
                        #arg,
                        sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
                    )
                ));
            }

            let final_value_opt = match args.inner.final_value_opt.as_ref() {
                Some(arg) => {
                    let arg = generate_attribute_arg_code(arg);
                    quote!(Some(Box::new(#arg)))
                }

                None => quote!(None),
            };

            quote!(Some(sway_ast::Parens {
                inner: sway_ast::Punctuated {
                    value_separator_pairs: vec![#(#value_separated_pairs),*],
                    final_value_opt: #final_value_opt,
                },
                span: sway_types::Span::dummy(),
            }))
        }

        None => quote!(None),
    };

    quote!(sway_ast::attribute::Attribute {
        name: #name,
        args: #args,
    })
}

fn generate_attribute_arg_code(arg: &sway_ast::attribute::AttributeArg) -> TokenStream {
    let name = arg.name.as_str();

    let value = match arg.value.as_ref() {
        Some(value) => {
            let value = generate_literal_code(value);
            quote!(Some(#value))
        }

        None => quote!(None),
    };

    quote!(sway_ast::attribute::AttributeArg {
        name: sway_types::BaseIdent::new_no_span(#name.into()),
        value: #value,
    })
}

fn generate_literal_code(literal: &sway_ast::Literal) -> TokenStream {
    match literal {
        sway_ast::Literal::String(sway_ast::literal::LitString { parsed, .. }) => {
            quote!(sway_ast::Literal::String(sway_ast::literal::LitString {
                span: sway_types::Span::dummy(),
                parsed: #parsed.into(),
            }))
        }

        sway_ast::Literal::Char(sway_ast::literal::LitChar { parsed, .. }) => {
            quote!(sway_ast::Literal::Char(sway_ast::literal::LitChar {
                span: sway_types::Span::dummy(),
                parsed: #parsed,
            }))
        }

        sway_ast::Literal::Int(sway_ast::literal::LitInt { parsed, ty_opt, .. }) => {
            let parsed = parsed.to_string();

            let ty_opt = match ty_opt.as_ref() {
                Some(ty) => {
                    let ty = match &ty.0 {
                        sway_ast::LitIntType::U8 => quote!(sway_ast::LitIntType::U8),
                        sway_ast::LitIntType::U16 => quote!(sway_ast::LitIntType::U16),
                        sway_ast::LitIntType::U32 => quote!(sway_ast::LitIntType::U32),
                        sway_ast::LitIntType::U64 => quote!(sway_ast::LitIntType::U64),
                        sway_ast::LitIntType::U256 => quote!(sway_ast::LitIntType::U256),
                        sway_ast::LitIntType::I8 => quote!(sway_ast::LitIntType::I8),
                        sway_ast::LitIntType::I16 => quote!(sway_ast::LitIntType::I16),
                        sway_ast::LitIntType::I32 => quote!(sway_ast::LitIntType::I32),
                        sway_ast::LitIntType::I64 => quote!(sway_ast::LitIntType::I64),
                    };

                    quote!(
                        Some(
                            (
                                #ty,
                                sway_types::Span::dummy()
                            )
                        )
                    )
                }

                None => quote!(None),
            };
            
            quote!(sway_ast::Literal::Int(sway_ast::literal::LitInt {
                span: sway_types::Span::dummy(),
                parsed: num_bigint::BigUint::from_str(#parsed).unwrap(),
                ty_opt: #ty_opt,
            }))
        }

        sway_ast::Literal::Bool(sway_ast::literal::LitBool { kind, .. }) => {
            let kind = match kind {
                sway_ast::literal::LitBoolType::True => quote!(sway_ast::literal::LitBoolType::True),
                sway_ast::literal::LitBoolType::False => quote!(sway_ast::literal::LitBoolType::False),
            };

            quote!(sway_ast::Literal::Bool(sway_ast::literal::LitBool {
                span: sway_types::Span::dummy(),
                kind: #kind,
            }))
        }
    }
}

fn generate_expr_code(expr: &sway_ast::Expr) -> TokenStream {
    match expr {
        sway_ast::Expr::Literal(literal) => {
            let literal = generate_literal_code(literal);
            quote!(sway_ast::Expr::Literal(#literal))
        }

        sway_ast::Expr::Path(path_expr) => {
            let path_expr = generate_path_expr_code(path_expr);
            quote!(sway_ast::Expr::Path(#path_expr))
        }

        sway_ast::Expr::Shl { lhs, shl_token: _, rhs } => {
            let lhs = generate_expr_code(lhs.as_ref());
            let rhs = generate_expr_code(rhs.as_ref());

            quote!(sway_ast::Expr::Shl {
                lhs: Box::new(#lhs),
                shl_token: sway_ast::keywords::ShlToken::new(sway_types::Span::dummy()),
                rhs: Box::new(#rhs),
            })
        }

        sway_ast::Expr::FuncApp{ func, args } => {
            let func = generate_expr_code(func.as_ref());
            let mut value_separator_pairs = vec![];

            for value_separator_pair in args.inner.value_separator_pairs.iter() {
                let tree = generate_expr_code(&value_separator_pair.0);

                value_separator_pairs.push(quote!(
                    (
                        #tree,
                        sway_ast::keywords::CommaToken::new(sway_types::Span::dummy())
                    )
                ));
            }

            let final_value_opt = match args.inner.final_value_opt.as_ref() {
                Some(tree) => {
                    let tree = generate_expr_code(tree);
                    quote!(Some(Box::new(#tree)))
                }

                None => quote!(None),
            };

            quote!(sway_ast::Expr::FuncApp {
                func: Box::new(#func),
                args: sway_ast::Parens {
                    inner: sway_ast::Punctuated {
                        value_separator_pairs: vec![#(#value_separator_pairs),*],
                        final_value_opt: #final_value_opt,
                    },
                    span: sway_types::Span::dummy(),
                },
            })          
        }

        _ => todo!("{expr:#?}")
    }
}
