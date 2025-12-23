//! Tests for project loading.

use rpl::analysis::{Context, DiagnosticKind};
use rpl::value::Value;
use rpl::AnalysisSession;
use rpl_project::{Project, ProjectIndex};
use std::path::PathBuf;

fn projects_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/projects")
}

#[test]
fn load_hello_project() {
    let project_dir = projects_dir().join("hello");
    let project = Project::load(&project_dir).expect("failed to load hello project");

    // Check manifest
    assert_eq!(project.manifest().project.name, "hello");
    assert_eq!(project.manifest().project.entry, "main");
    assert_eq!(project.manifest().project.version, Some("0.1.0".to_string()));
    assert_eq!(
        project.manifest().project.author,
        Some("Test Author".to_string())
    );

    // Check main is loaded
    let main = project.session().vm().directory.lookup("main");
    assert!(main.is_some(), "main should be in directory");
    assert!(matches!(main.unwrap(), Value::Program(_)));
}

#[test]
fn hello_project_has_lib_functions() {
    let project_dir = projects_dir().join("hello");
    let project = Project::load(&project_dir).expect("failed to load");

    // Check lib/square is loaded
    let square = project.session().vm().directory.lookup("lib/square");
    assert!(square.is_some(), "lib/square should be in directory");
    assert!(matches!(square.unwrap(), Value::Program(_)));

    // Check lib/double is loaded
    let double = project.session().vm().directory.lookup("lib/double");
    assert!(double.is_some(), "lib/double should be in directory");
    assert!(matches!(double.unwrap(), Value::Program(_)));
}

#[test]
fn hello_project_has_constants() {
    let project_dir = projects_dir().join("hello");
    let project = Project::load(&project_dir).expect("failed to load");

    // Check constants list is loaded
    let constants = project.session().vm().directory.lookup("constants");
    assert!(constants.is_some(), "constants should be in directory");

    match constants.unwrap() {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::Integer(42));
            // items[1] is Real
            assert_eq!(items[2], Value::String("hello".into()));
        }
        other => panic!("expected List, got {:?}", other),
    }
}

#[test]
fn run_hello_project() {
    let project_dir = projects_dir().join("hello");
    let mut project = Project::load(&project_dir).expect("failed to load");

    // main computes: 3 lib/square 4 lib/square + = 9 + 16 = 25
    let result = project.run().expect("failed to run");
    assert_eq!(result, vec![Value::Integer(25)]);
}

#[test]
fn call_lib_function_directly() {
    let project_dir = projects_dir().join("hello");
    let mut project = Project::load(&project_dir).expect("failed to load");

    // Call lib/double directly: 21 lib/double = 42
    let result = project
        .session_mut()
        .eval("21 lib/double")
        .expect("failed to eval");
    assert_eq!(result, vec![Value::Integer(42)]);
}

#[test]
fn use_constants_in_eval() {
    let project_dir = projects_dir().join("hello");
    let mut project = Project::load(&project_dir).expect("failed to load");

    // Get first element of constants list: constants 1 GET = 42
    let result = project
        .session_mut()
        .eval("constants 1 GET")
        .expect("failed to eval");
    assert_eq!(result, vec![Value::Integer(42)]);
}

#[test]
fn context_filters_undefined_variable_for_project_entries() {
    let project_dir = projects_dir().join("hello");
    let index = ProjectIndex::build(&project_dir).expect("failed to build index");

    // Build context from project
    let context = index.to_context();

    // Verify lib/square is in the context
    assert!(
        context.is_known("lib/square"),
        "lib/square should be in context"
    );
    assert!(
        context.is_known("lib/double"),
        "lib/double should be in context"
    );
    assert!(
        context.is_known("constants"),
        "constants should be in context"
    );

    // Create analysis session with project context
    let mut session = AnalysisSession::new();
    rpl_stdlib::register_interfaces(session.interfaces_mut());
    session.set_context(context);

    // Analyze main.rpl content (references lib/square)
    let main_source = std::fs::read_to_string(project_dir.join("main.rpl")).unwrap();
    let id = session.set_source("main.rpl", &main_source);

    // Get diagnostics
    let diagnostics = session.diagnostics(id);

    // Should have NO undefined variable errors for lib/square
    let undefined_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| matches!(d.kind, DiagnosticKind::UndefinedVariable))
        .collect();

    assert!(
        undefined_errors.is_empty(),
        "Should have no undefined variable errors, got: {:?}",
        undefined_errors
    );
}

#[test]
fn context_still_reports_truly_undefined_variables() {
    let project_dir = projects_dir().join("hello");
    let index = ProjectIndex::build(&project_dir).expect("failed to build index");
    let context = index.to_context();

    let mut session = AnalysisSession::new();
    rpl_stdlib::register_interfaces(session.interfaces_mut());
    session.set_context(context);

    // Analyze code that references a truly undefined variable
    let id = session.set_source("test.rpl", "<< nonexistent_var >>");

    let diagnostics = session.diagnostics(id);

    // Should still report nonexistent_var as undefined
    let undefined_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| matches!(d.kind, DiagnosticKind::UndefinedVariable))
        .filter(|d| d.message.contains("nonexistent_var"))
        .collect();

    assert!(
        !undefined_errors.is_empty(),
        "Should report nonexistent_var as undefined"
    );
}
