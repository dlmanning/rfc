//! Project manifest (project.toml) parsing.

use crate::error::ManifestError;
use serde::Deserialize;
use std::path::Path;

/// A parsed project manifest.
#[derive(Debug, Clone, Deserialize)]
pub struct Manifest {
    /// Project metadata.
    pub project: ProjectSection,

    /// Build configuration.
    #[serde(default)]
    pub build: BuildSection,

    /// Extra sections not parsed by rpl-project.
    /// Runtimes can access these for custom configuration (e.g., `[sprites.*]`).
    #[serde(flatten)]
    pub extra: toml::Table,
}

/// The `[project]` section of the manifest.
#[derive(Debug, Clone, Deserialize)]
pub struct ProjectSection {
    /// Project name (required).
    pub name: String,

    /// Entry point program name, without .rpl extension (optional).
    /// Some runtimes use convention-based entry points (init/draw/update).
    #[serde(default)]
    pub entry: Option<String>,

    /// Project version (optional).
    #[serde(default)]
    pub version: Option<String>,

    /// Project author (optional).
    #[serde(default)]
    pub author: Option<String>,
}

/// The `[build]` section of the manifest.
#[derive(Debug, Clone, Deserialize)]
pub struct BuildSection {
    /// Glob patterns for files to include (default: `["**/*.rpl"]`).
    #[serde(default = "default_include")]
    pub include: Vec<String>,

    /// Glob patterns for files to exclude (default: `[]`).
    #[serde(default)]
    pub exclude: Vec<String>,
}

fn default_include() -> Vec<String> {
    vec!["**/*.rpl".to_string()]
}

impl Default for BuildSection {
    fn default() -> Self {
        Self {
            include: default_include(),
            exclude: Vec::new(),
        }
    }
}

impl Manifest {
    /// Load a manifest from a file path.
    pub fn from_file(path: &Path) -> Result<Self, ManifestError> {
        let content = std::fs::read_to_string(path).map_err(|e| ManifestError::Io {
            path: path.to_owned(),
            source: e,
        })?;

        Self::from_str(&content, path)
    }

    /// Parse a manifest from a string.
    pub fn from_str(content: &str, path: &Path) -> Result<Self, ManifestError> {
        let manifest: Manifest =
            toml::from_str(content).map_err(|e| ManifestError::Parse {
                path: path.to_owned(),
                source: e,
            })?;

        manifest.validate(path)?;
        Ok(manifest)
    }

    /// Validate the manifest has all required fields.
    fn validate(&self, path: &Path) -> Result<(), ManifestError> {
        if self.project.name.is_empty() {
            return Err(ManifestError::MissingField {
                path: path.to_owned(),
                field: "project.name",
            });
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn test_path() -> PathBuf {
        PathBuf::from("test/project.toml")
    }

    #[test]
    fn parse_minimal_manifest() {
        let content = r#"
            [project]
            name = "my-project"
        "#;

        let manifest = Manifest::from_str(content, &test_path()).unwrap();
        assert_eq!(manifest.project.name, "my-project");
        assert_eq!(manifest.project.entry, None);
        assert_eq!(manifest.build.include, vec!["**/*.rpl"]);
        assert!(manifest.build.exclude.is_empty());
    }

    #[test]
    fn parse_full_manifest() {
        let content = r#"
            [project]
            name = "my-game"
            entry = "main"
            version = "1.0.0"
            author = "Developer"

            [build]
            include = ["src/**/*.rpl", "lib/**/*.rpl"]
            exclude = ["test/**"]
        "#;

        let manifest = Manifest::from_str(content, &test_path()).unwrap();
        assert_eq!(manifest.project.name, "my-game");
        assert_eq!(manifest.project.entry, Some("main".to_string()));
        assert_eq!(manifest.project.version, Some("1.0.0".to_string()));
        assert_eq!(manifest.project.author, Some("Developer".to_string()));
        assert_eq!(manifest.build.include, vec!["src/**/*.rpl", "lib/**/*.rpl"]);
        assert_eq!(manifest.build.exclude, vec!["test/**"]);
    }

    #[test]
    fn missing_name_fails() {
        let content = r#"
            [project]
            name = ""
        "#;

        let result = Manifest::from_str(content, &test_path());
        assert!(matches!(
            result,
            Err(ManifestError::MissingField { field: "project.name", .. })
        ));
    }

    #[test]
    fn invalid_toml_fails() {
        let content = "this is not valid toml [[[";

        let result = Manifest::from_str(content, &test_path());
        assert!(matches!(result, Err(ManifestError::Parse { .. })));
    }

    #[test]
    fn extra_sections_preserved() {
        let content = r#"
            [project]
            name = "my-game"

            [sprites.sheet]
            image = "sheet.png"

            [sprites.sheet.regions]
            player = { x = 0, y = 0, w = 32, h = 32 }
            enemy = { x = 32, y = 0, w = 32, h = 32 }
        "#;

        let manifest = Manifest::from_str(content, &test_path()).unwrap();
        assert_eq!(manifest.project.name, "my-game");

        // Extra sections should be preserved
        let sprites = manifest.extra.get("sprites").expect("sprites section should exist");
        let sheet = sprites.get("sheet").expect("sheet should exist");
        assert_eq!(sheet.get("image").and_then(|v| v.as_str()), Some("sheet.png"));

        let regions = sheet.get("regions").expect("regions should exist");
        let player = regions.get("player").expect("player region should exist");
        assert_eq!(player.get("x").and_then(|v| v.as_integer()), Some(0));
        assert_eq!(player.get("w").and_then(|v| v.as_integer()), Some(32));
    }
}
