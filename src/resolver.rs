#[derive(Debug)]
pub struct AstModule {
    pub name: String,
    pub inner: sway_ast::Module,
}

#[derive(Debug)]
pub struct AstLibrary {
    pub name: String,
    pub modules: Vec<AstModule>,
}

#[derive(Debug)]
pub struct AstResolver {
    pub libraries: Vec<AstLibrary>,
}

impl AstResolver {
    pub fn new<P1: AsRef<std::path::Path>, P2: AsRef<std::path::Path>>(
        core_path: P1,
        std_path: P2,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            libraries: vec![
                AstLibrary {
                    name: "core".into(),
                    modules: parse_ast_modules(core_path)?,
                },
                AstLibrary {
                    name: "std".into(),
                    modules: parse_ast_modules(std_path)?,
                },
            ],
        })
    }
}

fn parse_ast_modules<P: AsRef<std::path::Path>>(path: P) -> Result<Vec<AstModule>, Box<dyn std::error::Error>> {
    let mut result = vec![];

    let path = path.as_ref().canonicalize()?;
    let path_string = path.to_string_lossy().to_string();

    for file in std::fs::read_dir(path).unwrap() {
        let file_path = file?.path();

        if file_path.is_dir() {
            result.extend(parse_ast_modules(file_path)?);
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
        let ast = sway_parse::parse_file(&handler, file.clone(), None).unwrap();

        let module_name = std::path::PathBuf::from(
            file_path
                .to_string_lossy()
                .trim_start_matches(&path_string)
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
            inner: ast.value,
        });
    }

    Ok(result)
}
