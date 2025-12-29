//! Tests for project loading.

use std::path::PathBuf;

use rpl::{AnalysisSession, analysis::DiagnosticKind, value::Value};
use rpl_project::{Project, ProjectIndex};

fn projects_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/projects")
}

#[test]
fn load_hello_project() {
    let project_dir = projects_dir().join("hello");
    let project = Project::load(&project_dir).expect("failed to load hello project");

    // Check manifest
    assert_eq!(project.manifest().project.name, "hello");
    assert_eq!(project.manifest().project.entry, Some(String::from("main")));
    assert_eq!(
        project.manifest().project.version,
        Some("0.1.0".to_string())
    );
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

    // Check lib.square is loaded (paths use . as separator)
    let square = project.session().vm().directory.lookup("lib.square");
    assert!(square.is_some(), "lib.square should be in directory");
    assert!(matches!(square.unwrap(), Value::Program(_)));

    // Check lib.double is loaded
    let double = project.session().vm().directory.lookup("lib.double");
    assert!(double.is_some(), "lib.double should be in directory");
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

    // main computes: 3 lib.square 4 lib.square + = 9 + 16 = 25
    let result = project.run().expect("failed to run");
    assert_eq!(result, vec![Value::Integer(25)]);
}

#[test]
fn call_lib_function_directly() {
    let project_dir = projects_dir().join("hello");
    let mut project = Project::load(&project_dir).expect("failed to load");

    // Call lib.double directly: 21 lib.double = 42
    let result = project
        .session_mut()
        .eval("21 lib.double")
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

    // Verify lib.square is in the context (paths use . as separator)
    assert!(
        context.is_known("lib.square"),
        "lib.square should be in context"
    );
    assert!(
        context.is_known("lib.double"),
        "lib.double should be in context"
    );
    assert!(
        context.is_known("constants"),
        "constants should be in context"
    );

    // Create analysis session with project context
    let mut session = AnalysisSession::new();
    rpl_stdlib::register_interfaces(session.interfaces_mut());
    session.set_context(context);

    // Analyze main.rpl content (references lib.square)
    let main_source = std::fs::read_to_string(project_dir.join("main.rpl")).unwrap();
    let id = session.set_source("main.rpl", &main_source);

    // Get diagnostics
    let diagnostics = session.diagnostics(id);

    // Should have NO undefined variable errors for lib.square
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

#[test]
fn context_includes_nested_function_definitions() {
    // Test that function definitions inside program bodies (like space-shooter's libs)
    // are properly recognized in the context.
    //
    // Files like lib/collision.rpl have this structure:
    // <<
    //   << body >> 'function_name' STO
    // >>
    let project_dir = projects_dir().join("nested-funcs");
    std::fs::create_dir_all(&project_dir).unwrap();

    // Create project.toml
    std::fs::write(
        project_dir.join("project.toml"),
        r#"
[project]
name = "nested-funcs"
entry = "main"
"#,
    )
    .unwrap();

    // Create lib directory
    std::fs::create_dir_all(project_dir.join("lib")).unwrap();

    // Create lib/utils.rpl with nested function definitions
    std::fs::write(
        project_dir.join("lib/utils.rpl"),
        r#"<<
  << DUP * >> 'square' STO
  << DUP + >> 'double' STO
>>"#,
    )
    .unwrap();

    // Create main.rpl that uses these functions
    std::fs::write(project_dir.join("main.rpl"), r#"<< 3 square 4 double + >>"#).unwrap();

    // Build index and verify globals are found
    let index = ProjectIndex::build(&project_dir).expect("failed to build index");

    // Verify the globals are collected
    let globals = index.globals();
    assert!(
        globals.contains_key("square"),
        "square should be in globals, found: {:?}",
        globals.keys().collect::<Vec<_>>()
    );
    assert!(
        globals.contains_key("double"),
        "double should be in globals, found: {:?}",
        globals.keys().collect::<Vec<_>>()
    );

    // Verify context includes these functions
    let context = index.to_context();
    assert!(context.is_known("square"), "square should be in context");
    assert!(context.is_known("double"), "double should be in context");

    // Clean up
    std::fs::remove_dir_all(&project_dir).ok();
}

#[test]
fn space_shooter_globals_are_recognized() {
    // Test that the space-shooter example project has its function definitions recognized
    let examples_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples");
    let project_dir = examples_dir.join("space-shooter");

    if !project_dir.exists() {
        // Skip if space-shooter doesn't exist
        return;
    }

    let index = ProjectIndex::build(&project_dir).expect("failed to build space-shooter index");

    // Verify key globals are found
    let globals = index.globals();

    // These functions are defined via STO in module files
    let expected_funcs = [
        // lib.entity defines these
        "spawn_entity",
        "mark_dead",
        "is_dead",
        // lib.helpers defines these
        "get_pos",
        "set_pos",
        "get_vel",
        "get_type",
        "get_sprite",
        // lib.collision defines these
        "check_collision",
        "check_collisions",
        // lib.utils defines these
        "is_offscreen",
        "clamp_to_screen",
        // systems.enemies defines these
        "spawn_enemy",
        "update_enemies",
        // systems.bullets defines these
        "spawn_bullet",
        "update_bullets",
        // Note: systems.player and systems.cleanup ARE the functions
        // (stored by path), not modules that define named functions
    ];

    for func_name in &expected_funcs {
        assert!(
            globals.contains_key(*func_name),
            "Expected global function '{}' not found in index. Found: {:?}",
            func_name,
            globals.keys().collect::<Vec<_>>()
        );
    }

    // Verify context includes these functions
    let context = index.to_context();
    for func_name in &expected_funcs {
        assert!(
            context.is_known(func_name),
            "Expected function '{}' not found in context",
            func_name
        );
    }
}

#[test]
fn spawn_entity_parameters_have_resolved_types() {
    // Test that spawn_entity's parameters get their types inferred from call sites
    let examples_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples");
    let project_dir = examples_dir.join("space-shooter");

    if !project_dir.exists() {
        return;
    }

    // Must register SR5 interfaces for PNGLOAD etc. to be recognized
    let index = ProjectIndex::build_with(&project_dir, |registry| {
        rpl_sr5::libraries::interfaces::register_interfaces(registry);
    })
    .expect("failed to build index");

    // Use the shared symbol table from the index (not per-file analysis)
    let symbols = index.symbols();

    // Find pos, vel, sprite, type definitions (the spawn_entity parameters)
    let params: Vec<_> = ["pos", "vel", "sprite", "type"]
        .iter()
        .map(|name| {
            symbols
                .definitions()
                .find(|d| d.name == *name)
                .map(|d| (d.name.clone(), d.value_type.clone()))
        })
        .collect();

    // Check that parameters have resolved types (not TypeVars)
    for (i, param) in params.iter().enumerate() {
        let name = ["pos", "vel", "sprite", "type"][i];
        if let Some((_, Some(ty))) = param {
            assert!(
                !ty.is_type_var(),
                "{} should not be a TypeVar, got {:?}",
                name,
                ty
            );
        }
    }
}
