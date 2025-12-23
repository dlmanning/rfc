//! Error types for project loading.

use std::path::PathBuf;
use thiserror::Error;

/// Errors that can occur when parsing a project manifest.
#[derive(Debug, Error)]
pub enum ManifestError {
    #[error("failed to read manifest at {path}: {source}")]
    Io {
        path: PathBuf,
        source: std::io::Error,
    },

    #[error("failed to parse manifest at {path}: {source}")]
    Parse {
        path: PathBuf,
        source: toml::de::Error,
    },

    #[error("missing required field '{field}' in {path}")]
    MissingField { path: PathBuf, field: &'static str },
}

/// Errors that can occur when loading a project.
#[derive(Debug, Error)]
pub enum LoadError {
    #[error(transparent)]
    Manifest(#[from] ManifestError),

    #[error("failed to read {path}: {source}")]
    Io {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("failed to evaluate {path}: {error}")]
    Eval { path: PathBuf, error: String },

    #[error("{path} produced no value (expected exactly one)")]
    NoValue { path: PathBuf },

    #[error("{path} produced {count} values (expected exactly one)")]
    MultipleValues { path: PathBuf, count: usize },

    #[error("invalid glob pattern '{pattern}': {source}")]
    Pattern {
        pattern: String,
        #[source]
        source: glob::PatternError,
    },

    #[error("failed to walk directory {path}: {source}")]
    WalkDir {
        path: PathBuf,
        #[source]
        source: walkdir::Error,
    },

    #[error("entry point '{entry}' not found in project at {project_dir}")]
    MissingEntry { entry: String, project_dir: PathBuf },

    #[error("path error: {0}")]
    StripPrefix(#[from] std::path::StripPrefixError),
}
