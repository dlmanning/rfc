//! RPL Project Loading
//!
//! This crate handles loading RPL projects from directories with `project.toml` manifests.
//!
//! # Project Structure
//!
//! ```text
//! my_project/
//! ├── project.toml          # Manifest
//! ├── main.rpl              # << ... entry point ... >>
//! └── lib/
//!     └── utils.rpl         # << ... utility functions ... >>
//! ```
//!
//! # Usage
//!
//! ```ignore
//! use rpl_project::Project;
//!
//! let mut project = Project::load("path/to/project")?;
//! project.run()?;
//! ```

pub mod error;
pub mod index;
pub mod loader;
pub mod manifest;

use std::path::Path;

pub use error::{LoadError, ManifestError};
pub use index::{IndexEntry, ProjectIndex, ValueType};
pub use manifest::Manifest;
use rpl::{Session, value::Value};

/// A loaded RPL project ready for execution.
pub struct Project {
    /// The session with loaded directory contents.
    pub session: Session,

    /// The project manifest.
    pub manifest: Manifest,
}

impl Project {
    /// Load a project from a directory using a new session with stdlib.
    ///
    /// This will:
    /// 1. Parse the manifest
    /// 2. Walk the directory for matching files
    /// 3. Load each file and store it in the VM directory
    /// 4. Verify the entry point exists
    pub fn load(project_dir: impl AsRef<Path>) -> Result<Self, LoadError> {
        let mut session = Session::new();
        rpl_stdlib::register_interfaces(session.interfaces_mut());
        rpl_stdlib::register_lowerers(session.lowerers_mut());
        rpl_stdlib::register_executors(session.executors_mut());

        Self::load_into(session, project_dir)
    }

    /// Load a project into an existing session.
    ///
    /// Use this when you need custom libraries registered before loading,
    /// such as platform-specific commands (e.g., SR5 graphics/input).
    ///
    /// All files are evaluated using the same session, so earlier files
    /// are visible to later files during loading.
    pub fn load_into(
        mut session: Session,
        project_dir: impl AsRef<Path>,
    ) -> Result<Self, LoadError> {
        let project_dir = project_dir.as_ref();

        // Load manifest
        let manifest_path = project_dir.join("project.toml");
        let manifest = Manifest::from_file(&manifest_path)?;

        // Collect files
        let files = loader::collect_files(
            project_dir,
            &manifest.build.include,
            &manifest.build.exclude,
        )?;

        // Separate binary and .rpl files - load binaries first so they're
        // available when .rpl files are evaluated
        let (rpl_files, binary_files): (Vec<_>, Vec<_>) = files
            .into_iter()
            .partition(|p| p.extension().and_then(|e| e.to_str()) == Some("rpl"));

        // Load binary files first
        for file_path in binary_files {
            let key = loader::path_to_key(project_dir, &file_path)?;
            let data = std::fs::read(&file_path).map_err(|e| LoadError::Io {
                path: file_path.clone(),
                source: e,
            })?;
            session
                .vm_mut()
                .directory
                .store(key, Value::Bytes(data.into()));
        }

        // Load .rpl files - evaluate each in the SAME session so files can
        // reference each other. Each file should produce exactly one value.
        for file_path in rpl_files {
            let key = loader::path_to_key(project_dir, &file_path)?;
            let source = std::fs::read_to_string(&file_path).map_err(|e| LoadError::Io {
                path: file_path.clone(),
                source: e,
            })?;

            // Evaluate the source - this executes the file content and leaves
            // the result on the stack (e.g., `<< ... >>` becomes a Program value)
            session.eval(&source).map_err(|e| LoadError::Eval {
                path: file_path.clone(),
                source: e,
            })?;

            // Pop the result and store it in the directory
            let value = session
                .vm_mut()
                .stack
                .pop()
                .map_err(|_| LoadError::NoValue {
                    path: file_path.clone(),
                })?;

            session.vm_mut().directory.store(key, value);
        }

        // Verify entry point exists (if specified)
        if let Some(entry) = &manifest.project.entry
            && session.vm().directory.lookup(entry).is_none()
        {
            return Err(LoadError::MissingEntry {
                entry: entry.clone(),
                project_dir: project_dir.to_owned(),
            });
        }

        Ok(Project { session, manifest })
    }

    /// Run the project's entry point program.
    ///
    /// This evaluates the entry point name, which looks it up in the directory
    /// and automatically executes it if it's a program.
    ///
    /// Returns an error if no entry point is specified in the manifest.
    pub fn run(&mut self) -> Result<Vec<rpl::value::Value>, LoadError> {
        let entry = self
            .manifest
            .project
            .entry
            .clone()
            .ok_or_else(|| LoadError::MissingEntry {
                entry: "<none>".to_string(),
                project_dir: std::path::PathBuf::new(),
            })?;
        self.session.eval(&entry).map_err(|e| LoadError::Eval {
            path: entry.into(),
            source: e,
        })
    }

    /// Get a reference to the session.
    pub fn session(&self) -> &Session {
        &self.session
    }

    /// Get a mutable reference to the session.
    pub fn session_mut(&mut self) -> &mut Session {
        &mut self.session
    }

    /// Get a reference to the manifest.
    pub fn manifest(&self) -> &Manifest {
        &self.manifest
    }
}
