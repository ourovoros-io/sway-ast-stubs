use std::fs::canonicalize;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("cargo::rerun-if-changed=build.rs");

    if !std::path::Path::new("./sway").exists() {
        let command = std::process::Command::new("git")
            .args(&["clone", "https://github.com/FuelLabs/sway.git"])
            .spawn()?;
        command.wait_with_output()?;
    }

    let mut exprs = vec![];
    exprs.push(quote::quote! {
        let mut core_library = HashMap::new();
    });

    exprs.extend(parse(
        "core",
        &std::path::PathBuf::from("./sway/sway-lib-core/src"),
    )?);

    exprs.push(quote::quote! {
        libraries.insert("core".to_string(), core_library);
    });

    exprs.push(quote::quote! {
        let mut std_library = HashMap::new();
    });

    exprs.extend(parse(
        "std",
        &std::path::PathBuf::from("./sway/sway-lib-std/src"),
    )?);

    exprs.push(quote::quote! {
        libraries.insert("std".to_string(), std_library);
    });

    
    let generated = quote::quote! {
        use std::collections::HashMap;

        pub struct AstResolver {
            pub libraries: HashMap<String, HashMap<String, HashMap<String, Vec<String>>>>,
        }

        impl Default for AstResolver {
            fn default() -> Self {
                let mut libraries = HashMap::new();

                #(#exprs)*

                AstResolver { libraries }
            }
        }
    };

    std::fs::write("./src/lib.rs", generated.to_string())?;

    Ok(())
}

pub fn parse(
    prefix: &str,
    path: &std::path::PathBuf,
) -> Result<Vec<TokenStream>, Box<dyn std::error::Error>> {
    let lib_id = format_ident!("{}_library", prefix);

    let mut exprs = vec![];

    let path = canonicalize(path)?;
    let path_string = path.to_string_lossy().to_string();

    for file in std::fs::read_dir(path).unwrap() {
        let file = file.unwrap();
        let file_path = file.path();

        if file_path.is_dir() {
            exprs.extend(parse(prefix, &file_path)?);
            continue;
        }
        if file_path
            .extension()
            .as_ref()
            .map(|x| x.to_string_lossy().to_string())
            .unwrap_or_default()
            != "sw"
        {
            continue;
        }

        let file_path_string = file_path.to_string_lossy().to_string();
        let module_name = file_path_string.trim_start_matches(&path_string);
        let module_name = module_name.strip_suffix(".sw").unwrap();
        let module_name = std::path::PathBuf::from(module_name);
        let module_name = module_name
            .components()
            .map(|b| {
                b.as_os_str()
                    .to_string_lossy()
                    .trim_start_matches("/")
                    .to_string()
            })
            .filter(|x| !x.is_empty())
            .collect::<Vec<_>>()
            .join("::");

        let module_id = format_ident!("{}_module", module_name);
        let mut modules_items = vec![];

        let file = std::fs::read_to_string(file_path.clone())?;
        let file: std::sync::Arc<str> = std::sync::Arc::from(file);
        let handler = sway_error::handler::Handler::default();
        let ast = sway_parse::parse_file(&handler, file.clone(), None).unwrap();

        let mut trait_found = false;

        for item in &ast.value.items {
            let sway_ast::ItemKind::Trait(trait_obj) = &item.value else {
                continue;
            };
            trait_found = true;
            modules_items.push(quote::quote! {
                let mut trait_item = vec![];
            });
            let trait_name = trait_obj.name.to_string();
            let mut methods = vec![];
            for item in &trait_obj.trait_items.inner {
                let sway_ast::ItemTraitItem::Fn(method, ..) = &item.value else {
                    continue;
                };
                let method = method.name.to_string();
                methods.push(quote! {
                    #method.to_string()
                });
            }

            modules_items.push(quote::quote! {
                trait_item.extend(vec![#(#methods),*]);
            });
            modules_items.push(quote::quote! {
                #module_id.insert(#trait_name.to_string(), trait_item);
            });
        }
        if !trait_found {
            continue;
        }
        if modules_items.is_empty() {
            continue;
        }
        exprs.push(quote::quote! {
            let mut #module_id = HashMap::new();
        });
        exprs.extend(modules_items);
        exprs.push(quote::quote! {
            #lib_id.insert(#module_name.to_string(), #module_id);
        });
    }
    
    Ok(exprs)
}
