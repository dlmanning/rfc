//! Meta-commands for the REPL (commands starting with colon).

use rpl_plot::register_plot_lib;

use super::app::App;

/// Result of executing a meta-command.
pub enum CommandResult {
    /// Command was handled.
    Handled,
    /// Not a command, should be executed as RPL code.
    NotACommand,
    /// Quit the REPL.
    Quit,
}

/// Try to execute input as a meta-command.
pub fn try_execute_command(app: &mut App, input: &str) -> CommandResult {
    let trimmed = input.trim();

    // Commands start with ':'
    if !trimmed.starts_with(':') {
        return CommandResult::NotACommand;
    }

    // Parse command and arguments
    let parts: Vec<&str> = trimmed[1..].split_whitespace().collect();
    if parts.is_empty() {
        return CommandResult::NotACommand;
    }

    let cmd = parts[0].to_lowercase();
    let args = &parts[1..];

    match cmd.as_str() {
        "help" | "h" | "?" => {
            show_help(app);
            CommandResult::Handled
        }
        "quit" | "q" | "exit" => CommandResult::Quit,
        "clear" | "cls" => {
            clear_stack(app);
            CommandResult::Handled
        }
        "clearall" | "ca" => {
            clear_all(app);
            CommandResult::Handled
        }
        "vars" | "v" => {
            show_vars(app);
            CommandResult::Handled
        }
        "stack" | "s" => {
            show_stack(app);
            CommandResult::Handled
        }
        "home" => {
            go_home(app);
            CommandResult::Handled
        }
        "cd" => {
            change_dir(app, args);
            CommandResult::Handled
        }
        _ => {
            app.last_error = Some(format!("Unknown command: :{}", cmd));
            CommandResult::Handled
        }
    }
}

fn show_help(app: &mut App) {
    app.last_error = Some(
        "Commands: :help :quit :clear :clearall :vars :stack :home :cd <dir>".to_string(),
    );
}

fn clear_stack(app: &mut App) {
    // Execute CLEAR command
    let _ = app.session.eval_repl("CLEAR");
    app.last_error = None;
}

fn clear_all(app: &mut App) {
    // Clear stack and all variables
    let _ = app.session.eval_repl("CLEAR");
    // Reset session for clean state
    app.session = rpl::Session::new();
    register_plot_lib(&mut app.session);
    app.last_error = Some("Stack and variables cleared".to_string());
}

fn show_vars(app: &mut App) {
    let names: Vec<&str> = app
        .session
        .vm()
        .directory
        .vars()
        .map(|name| name.as_str())
        .collect();
    if names.is_empty() {
        app.last_error = Some("No variables defined".to_string());
    } else {
        app.last_error = Some(format!("Variables: {}", names.join(", ")));
    }
}

fn show_stack(app: &mut App) {
    let stack = app.session.vm().stack_contents();
    if stack.is_empty() {
        app.last_error = Some("Stack is empty".to_string());
    } else {
        app.last_error = Some(format!("Stack depth: {}", stack.len()));
    }
}

fn go_home(app: &mut App) {
    // Execute HOME command
    let _ = app.session.eval_repl("HOME");
    app.last_error = None;
}

fn change_dir(app: &mut App, args: &[&str]) {
    if args.is_empty() {
        // Go home
        let _ = app.session.eval_repl("HOME");
    } else if args[0] == ".." {
        // Go up
        let _ = app.session.eval_repl("UPDIR");
    } else {
        // Enter directory
        let dir_name = args[0];
        let code = format!("\"{}\" PGDIR", dir_name);
        if let Err(e) = app.session.eval_repl(&code) {
            app.last_error = Some(format!("Cannot enter directory: {}", e));
            return;
        }
    }
    app.last_error = None;
}
