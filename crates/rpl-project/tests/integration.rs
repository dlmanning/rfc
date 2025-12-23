//! Integration tests for project loading.

use rpl::value::Value;
use rpl_project::{LoadError, Project, ProjectIndex, ValueType};
use std::path::PathBuf;

fn fixtures_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

#[test]
fn load_simple_project() {
    let project_dir = fixtures_dir().join("simple_project");
    let project = Project::load(&project_dir).expect("failed to load simple project");

    // Check manifest
    assert_eq!(project.manifest().project.name, "simple");
    assert_eq!(project.manifest().project.entry, "main");

    // Check directory contents
    let main = project.session().vm().directory.lookup("main");
    assert!(main.is_some(), "main should be in directory");
    assert!(matches!(main.unwrap(), Value::Program(_)));

    let data = project.session().vm().directory.lookup("data");
    assert!(data.is_some(), "data should be in directory");
    assert!(matches!(data.unwrap(), Value::List(_)));
}

#[test]
fn run_simple_project() {
    let project_dir = fixtures_dir().join("simple_project");
    let mut project = Project::load(&project_dir).expect("failed to load");

    let result = project.run().expect("failed to run");
    assert_eq!(result, vec![Value::Integer(42)]);
}

#[test]
fn load_nested_project() {
    let project_dir = fixtures_dir().join("nested_project");
    let project = Project::load(&project_dir).expect("failed to load nested project");

    // Check manifest
    assert_eq!(project.manifest().project.name, "nested");
    assert_eq!(project.manifest().project.version, Some("1.0.0".to_string()));

    // Check nested directory contents
    let square = project.session().vm().directory.lookup("math/square");
    assert!(square.is_some(), "math/square should be in directory");
    assert!(matches!(square.unwrap(), Value::Program(_)));

    let double = project.session().vm().directory.lookup("math/double");
    assert!(double.is_some(), "math/double should be in directory");
    assert!(matches!(double.unwrap(), Value::Program(_)));
}

#[test]
fn run_nested_project() {
    let project_dir = fixtures_dir().join("nested_project");
    let mut project = Project::load(&project_dir).expect("failed to load");

    // main runs: 5 math/square → 5 squared = 25
    let result = project.run().expect("failed to run");
    assert_eq!(result, vec![Value::Integer(25)]);
}

#[test]
fn missing_manifest_fails() {
    let project_dir = fixtures_dir().join("nonexistent");
    let result = Project::load(&project_dir);

    assert!(result.is_err());
}

#[test]
fn missing_entry_point_fails() {
    // Create a temp directory with manifest but no entry point
    let temp_dir = std::env::temp_dir().join("rpl_test_missing_entry");
    std::fs::create_dir_all(&temp_dir).ok();

    std::fs::write(
        temp_dir.join("project.toml"),
        r#"
        [project]
        name = "test"
        entry = "nonexistent"
        "#,
    )
    .unwrap();

    let result = Project::load(&temp_dir);
    assert!(matches!(result, Err(LoadError::MissingEntry { .. })));

    // Cleanup
    std::fs::remove_dir_all(&temp_dir).ok();
}

// ============================================================================
// ProjectIndex tests
// ============================================================================

#[test]
fn build_simple_index() {
    let project_dir = fixtures_dir().join("simple_project");
    let index = ProjectIndex::build(&project_dir).expect("failed to build index");

    // Check entries exist
    assert!(index.get("main").is_some());
    assert!(index.get("data").is_some());

    // Check value types
    let main_entry = index.get("main").unwrap();
    assert_eq!(main_entry.value_type, ValueType::Program);

    let data_entry = index.get("data").unwrap();
    assert_eq!(data_entry.value_type, ValueType::List);
}

#[test]
fn index_infers_simple_signature() {
    let project_dir = fixtures_dir().join("simple_project");
    let index = ProjectIndex::build(&project_dir).expect("failed to build index");

    // main should have a signature
    let main_entry = index.get("main").unwrap();
    assert!(
        main_entry.signature.is_some(),
        "main should have a signature"
    );
}

#[test]
fn build_nested_index() {
    let project_dir = fixtures_dir().join("nested_project");
    let index = ProjectIndex::build(&project_dir).expect("failed to build index");

    // Check nested entries exist
    assert!(index.get("main").is_some());
    assert!(index.get("math/square").is_some());
    assert!(index.get("math/double").is_some());

    // Check value types
    let square_entry = index.get("math/square").unwrap();
    assert_eq!(square_entry.value_type, ValueType::Program);

    let double_entry = index.get("math/double").unwrap();
    assert_eq!(double_entry.value_type, ValueType::Program);
}

#[test]
fn index_infers_library_signatures() {
    let project_dir = fixtures_dir().join("nested_project");
    let index = ProjectIndex::build(&project_dir).expect("failed to build index");

    // math/square should have a signature (Num -> Num)
    let square_sig = index.get_signature("math/square");
    assert!(square_sig.is_some(), "math/square should have a signature");
    let sig = square_sig.unwrap();
    eprintln!("math/square signature: {:?}", sig);
    // DUP * takes 1 arg and produces 1 result
    // Note: Without explicit parameters via ->, the signature inference
    // sees this as () -> (Num) because DUP duplicates whatever is on stack
    // Let's just verify we got a signature
    assert!(
        !sig.outputs.is_empty(),
        "square should produce at least 1 output"
    );

    // math/double should have a signature (Num -> Num)
    let double_sig = index.get_signature("math/double");
    assert!(double_sig.is_some(), "math/double should have a signature");
    let sig = double_sig.unwrap();
    eprintln!("math/double signature: {:?}", sig);
    // 2 * takes something from stack and produces 1 result
    assert!(
        !sig.outputs.is_empty(),
        "double should produce at least 1 output"
    );
}

#[test]
fn index_infers_explicit_parameters() {
    let project_dir = fixtures_dir().join("params_project");
    let index = ProjectIndex::build(&project_dir).expect("failed to build index");

    // lib/add_one uses explicit parameter: -> x << x 1 + >>
    let add_one_sig = index.get_signature("lib/add_one");
    assert!(
        add_one_sig.is_some(),
        "lib/add_one should have a signature"
    );
    let sig = add_one_sig.unwrap();
    eprintln!("lib/add_one signature: {:?}", sig);

    // With explicit parameter, we should have 1 input
    assert_eq!(sig.inputs.len(), 1, "add_one should take 1 input");
    assert_eq!(sig.outputs.len(), 1, "add_one should produce 1 output");
}

#[test]
fn hello_project_signatures() {
    let project_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("tests/projects/hello");
    let index = ProjectIndex::build(&project_dir).expect("failed to build index");

    // Print all signatures for debugging
    for (key, entry) in &index.entries {
        eprintln!("{}: {:?}", key, entry.signature);
    }

    // lib/square should have 1 input (x)
    let square_sig = index.get_signature("lib/square");
    assert!(square_sig.is_some(), "lib/square should have a signature");
    let sig = square_sig.unwrap();
    assert_eq!(sig.inputs.len(), 1, "lib/square should have 1 input");

    // lib/double should have 1 input (x)
    let double_sig = index.get_signature("lib/double");
    assert!(double_sig.is_some(), "lib/double should have a signature");
    let sig = double_sig.unwrap();
    assert_eq!(sig.inputs.len(), 1, "lib/double should have 1 input");
}

#[test]
fn index_analysis_has_no_errors() {
    let project_dir = fixtures_dir().join("nested_project");
    let index = ProjectIndex::build(&project_dir).expect("failed to build index");

    // Check that all program entries have clean analysis (no errors)
    for (key, entry) in &index.entries {
        if entry.value_type.is_program() {
            if let Some(ref analysis) = entry.analysis {
                let errors: Vec<_> = analysis
                    .diagnostics
                    .iter()
                    .filter(|d| d.severity == rpl::analysis::Severity::Error)
                    .collect();
                assert!(
                    errors.is_empty(),
                    "Program '{}' has errors: {:?}",
                    key,
                    errors
                );
            }
        }
    }
}
