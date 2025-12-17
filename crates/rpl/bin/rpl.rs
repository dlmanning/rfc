//! RPL command-line interpreter.
//!
//! Usage:
//!   rpl <file.rpl>     Evaluate a file
//!   rpl -e <code>      Evaluate a string
//!   rpl               Read from stdin

use std::{
    env, fs,
    io::{self, Read},
    process::ExitCode,
};

use rpl::Session;

const USAGE: &str = "\
Usage: rpl [OPTIONS] [FILE]

Arguments:
  [FILE]  RPL source file to evaluate

Options:
  -e <CODE>  Evaluate CODE string
  -h, --help Print this help message

If no arguments are given, reads from stdin.";

fn read_stdin() -> Result<String, io::Error> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;
    Ok(buf)
}

enum Action {
    Eval(String),
    Help,
}

fn parse_args() -> Result<Action, String> {
    let args: Vec<String> = env::args().skip(1).collect();

    match args.as_slice() {
        [] => Ok(Action::Eval(
            read_stdin().map_err(|e| format!("error reading stdin: {e}"))?,
        )),
        [arg] if arg == "-" => Ok(Action::Eval(
            read_stdin().map_err(|e| format!("error reading stdin: {e}"))?,
        )),
        [arg] if arg == "-h" || arg == "--help" => Ok(Action::Help),
        [flag, code] if flag == "-e" => Ok(Action::Eval(code.clone())),
        [file] => Ok(Action::Eval(
            fs::read_to_string(file).map_err(|e| format!("error reading {file}: {e}"))?,
        )),
        _ => Err(USAGE.into()),
    }
}

fn main() -> ExitCode {
    match parse_args() {
        Ok(Action::Help) => {
            println!("{USAGE}");
            ExitCode::SUCCESS
        }
        Ok(Action::Eval(source)) => {
            let mut session = Session::new();
            match session.eval(&source) {
                Ok(stack) => {
                    for value in &stack {
                        println!("{value}");
                    }
                    ExitCode::SUCCESS
                }
                Err(e) => {
                    eprintln!("{e}");
                    ExitCode::FAILURE
                }
            }
        }
        Err(e) => {
            eprintln!("{e}");
            ExitCode::FAILURE
        }
    }
}
