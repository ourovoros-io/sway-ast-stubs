use std::{
    error::Error,
    path::{Path, PathBuf},
    str::FromStr,
};

// NOTE: We use the `include!` macro because the `AstResolver` structure is used by both the build script and the crate itself.
include!("resolver.rs");
include!("default.rs");

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_default_resolver() {
        let resolver = AstResolver::default();
        println!("{resolver:#?}");
    }
}
