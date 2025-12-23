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

pub use error::{LoadError, ManifestError};
pub use index::{IndexEntry, ProjectIndex, ValueType};
pub use manifest::Manifest;

use rpl::Session;
use std::path::Path;

/// A loaded RPL project ready for execution.
pub struct Project {
    /// The session with loaded directory contents.
    pub session: Session,

    /// The project manifest.
    pub manifest: Manifest,
}

impl Project {
    /// Load a project from a directory containing `project.toml`.
    ///
    /// This will:
    /// 1. Parse the manifest
    /// 2. Walk the directory for `.rpl` files
    /// 3. Load each file and store it in the VM directory
    /// 4. Verify the entry point exists
    pub fn load(project_dir: impl AsRef<Path>) -> Result<Self, LoadError> {
        let project_dir = project_dir.as_ref();

        // 1. Load manifest
        let manifest_path = project_dir.join("project.toml");
        let manifest = Manifest::from_file(&manifest_path)?;

        // 2. Create session with stdlib
        let mut session = Session::new();
        rpl_stdlib::register_interfaces(session.interfaces_mut());
        rpl_stdlib::register_lowerers(session.lowerers_mut());
        rpl_stdlib::register_executors(session.executors_mut());

        // 3. Collect files
        let files = loader::collect_files(
            project_dir,
            &manifest.build.include,
            &manifest.build.exclude,
        )?;

        // 4. Load each file into directory
        for file_path in files {
            let key = loader::path_to_key(project_dir, &file_path)?;
            let value = loader::load_file(&file_path)?;

            // Store in VM directory
            session.vm_mut().directory.store(key, value);
        }

        // 5. Verify entry point exists
        let entry = &manifest.project.entry;
        if session.vm().directory.lookup(entry).is_none() {
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
    pub fn run(&mut self) -> Result<Vec<rpl::value::Value>, LoadError> {
        let entry = self.manifest.project.entry.clone();
        self.session.eval(&entry).map_err(|e| LoadError::Eval {
            path: entry.into(),
            error: format!("{:?}", e),
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
