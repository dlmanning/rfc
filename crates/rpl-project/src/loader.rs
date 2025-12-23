//! File and directory loading for projects.

use crate::error::LoadError;
use glob::Pattern;
use rpl::value::Value;
use rpl::Session;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

/// Load a single .rpl file and return the resulting Value.
///
/// The file must contain exactly one value literal.
/// Uses a temporary Session to parse, compile, and execute.
pub fn load_file(path: &Path) -> Result<Value, LoadError> {
    let source = std::fs::read_to_string(path).map_err(|e| LoadError::Io {
        path: path.to_owned(),
        source: e,
    })?;

    load_source(&source, path)
}

/// Load RPL source and return the resulting Value.
///
/// The source must produce exactly one value on the stack.
pub fn load_source(source: &str, path: &Path) -> Result<Value, LoadError> {
    // Create a fresh session with stdlib
    let mut session = Session::new();
    rpl_stdlib::register_interfaces(session.interfaces_mut());
    rpl_stdlib::register_lowerers(session.lowerers_mut());
    rpl_stdlib::register_executors(session.executors_mut());

    // Evaluate the source
    let values = session.eval(source).map_err(|e| LoadError::Eval {
        path: path.to_owned(),
        error: format!("{:?}", e),
    })?;

    // Must produce exactly one value
    match values.len() {
        0 => Err(LoadError::NoValue {
            path: path.to_owned(),
        }),
        1 => Ok(values.into_iter().next().unwrap()),
        n => Err(LoadError::MultipleValues {
            path: path.to_owned(),
            count: n,
        }),
    }
}

/// Collect all files matching include patterns, excluding exclude patterns.
///
/// Skips `project.toml` automatically.
pub fn collect_files(
    project_dir: &Path,
    include: &[String],
    exclude: &[String],
) -> Result<Vec<PathBuf>, LoadError> {
    // Compile patterns
    let include_patterns: Vec<Pattern> = include
        .iter()
        .map(|p| {
            Pattern::new(p).map_err(|e| LoadError::Pattern {
                pattern: p.clone(),
                source: e,
            })
        })
        .collect::<Result<_, _>>()?;

    let exclude_patterns: Vec<Pattern> = exclude
        .iter()
        .map(|p| {
            Pattern::new(p).map_err(|e| LoadError::Pattern {
                pattern: p.clone(),
                source: e,
            })
        })
        .collect::<Result<_, _>>()?;

    let mut files = Vec::new();

    // Walk directory
    for entry in WalkDir::new(project_dir) {
        let entry = entry.map_err(|e| LoadError::WalkDir {
            path: project_dir.to_owned(),
            source: e,
        })?;

        if !entry.file_type().is_file() {
            continue;
        }

        let rel_path = entry
            .path()
            .strip_prefix(project_dir)
            .map_err(LoadError::StripPrefix)?;

        // Use forward slashes for glob matching on all platforms
        let rel_str = rel_path.to_string_lossy().replace('\\', "/");

        // Skip project.toml
        if rel_str == "project.toml" {
            continue;
        }

        // Check include patterns
        let included = include_patterns.iter().any(|p| p.matches(&rel_str));
        if !included {
            continue;
        }

        // Check exclude patterns
        let excluded = exclude_patterns.iter().any(|p| p.matches(&rel_str));
        if excluded {
            continue;
        }

        files.push(entry.path().to_owned());
    }

    // Sort for deterministic loading order
    files.sort();

    Ok(files)
}

/// Convert file path to directory key.
///
/// `project/math/square.rpl` → `math/square`
///
/// Uses forward slashes as path separators regardless of platform.
pub fn path_to_key(project_dir: &Path, file_path: &Path) -> Result<String, LoadError> {
    let rel_path = file_path
        .strip_prefix(project_dir)
        .map_err(LoadError::StripPrefix)?;

    let without_ext = rel_path.with_extension("");

    // Use forward slashes for consistency
    Ok(without_ext.to_string_lossy().replace('\\', "/"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn load_integer() {
        let value = load_source("42", &PathBuf::from("test.rpl")).unwrap();
        assert_eq!(value, Value::Integer(42));
    }

    #[test]
    fn load_real() {
        let value = load_source("3.14159", &PathBuf::from("test.rpl")).unwrap();
        match value {
            Value::Real(r) => assert!((r - 3.14159).abs() < 1e-10),
            _ => panic!("expected Real"),
        }
    }

    #[test]
    fn load_string() {
        let value = load_source("\"hello\"", &PathBuf::from("test.rpl")).unwrap();
        assert_eq!(value, Value::String("hello".into()));
    }

    #[test]
    fn load_list() {
        let value = load_source("{ 1 2 3 }", &PathBuf::from("test.rpl")).unwrap();
        match value {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::Integer(1));
                assert_eq!(items[1], Value::Integer(2));
                assert_eq!(items[2], Value::Integer(3));
            }
            _ => panic!("expected List"),
        }
    }

    #[test]
    fn load_program() {
        let value = load_source("<< 2 * >>", &PathBuf::from("test.rpl")).unwrap();
        assert!(matches!(value, Value::Program(_)));
    }

    #[test]
    fn load_empty_fails() {
        let result = load_source("", &PathBuf::from("test.rpl"));
        assert!(matches!(result, Err(LoadError::NoValue { .. })));
    }

    #[test]
    fn load_multiple_values_fails() {
        let result = load_source("1 2 3", &PathBuf::from("test.rpl"));
        assert!(matches!(
            result,
            Err(LoadError::MultipleValues { count: 3, .. })
        ));
    }

    #[test]
    fn path_to_key_simple() {
        let project = PathBuf::from("/project");
        let file = PathBuf::from("/project/main.rpl");
        assert_eq!(path_to_key(&project, &file).unwrap(), "main");
    }

    #[test]
    fn path_to_key_nested() {
        let project = PathBuf::from("/project");
        let file = PathBuf::from("/project/math/square.rpl");
        assert_eq!(path_to_key(&project, &file).unwrap(), "math/square");
    }

    #[test]
    fn path_to_key_deeply_nested() {
        let project = PathBuf::from("/project");
        let file = PathBuf::from("/project/lib/math/trig/sin.rpl");
        assert_eq!(path_to_key(&project, &file).unwrap(), "lib/math/trig/sin");
    }
}
