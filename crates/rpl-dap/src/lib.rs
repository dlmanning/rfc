//! DAP (Debug Adapter Protocol) Server implementation.
//!
//! This module provides a Debug Adapter Protocol server for RPL
//! that communicates over stdio, enabling debugging in VS Code
//! and other DAP-compatible editors.

mod handlers;
mod session;

use dap::prelude::*;
use std::io::{self, BufReader, BufWriter, Stdin, Stdout};

pub use session::DebugSession;

use rpl_session::LibraryRegistry;
use rpl_stdlib::{register_standard_libs, register_standard_operators};
use rpl_session::OperatorRegistry;

/// DAP Server for stdio communication.
pub struct DapServer {
    server: Server<Stdin, Stdout>,
    session: Option<DebugSession>,
    registry: LibraryRegistry,
    operators: OperatorRegistry,
}

impl DapServer {
    /// Create a new DAP server using stdio.
    pub fn new() -> Self {
        let stdin = io::stdin();
        let stdout = io::stdout();
        let input = BufReader::new(stdin);
        let output = BufWriter::new(stdout);

        let mut registry = LibraryRegistry::new();
        register_standard_libs(&mut registry);
        let mut operators = OperatorRegistry::new();
        register_standard_operators(&mut operators);

        Self {
            server: Server::new(input, output),
            session: None,
            registry,
            operators,
        }
    }

    /// Run the server main loop.
    pub fn run(&mut self) -> Result<(), DapError> {
        loop {
            let req = match self.server.poll_request()? {
                Some(req) => req,
                None => return Ok(()), // EOF
            };

            match req.command.clone() {
                Command::Initialize(args) => {
                    handlers::handle_initialize(&mut self.server, req, &args)?;
                }

                Command::Launch(args) => {
                    handlers::handle_launch(
                        &mut self.server,
                        &mut self.session,
                        req,
                        &args,
                    )?;
                }

                Command::SetBreakpoints(args) => {
                    handlers::handle_set_breakpoints(
                        &mut self.server,
                        &mut self.session,
                        req,
                        &args,
                    )?;
                }

                Command::ConfigurationDone => {
                    handlers::handle_configuration_done(
                        &mut self.server,
                        &mut self.session,
                        &self.registry,
                        &self.operators,
                        req,
                    )?;
                }

                Command::Threads => {
                    handlers::handle_threads(&mut self.server, req)?;
                }

                Command::StackTrace(args) => {
                    handlers::handle_stack_trace(
                        &mut self.server,
                        &self.session,
                        req,
                        &args,
                    )?;
                }

                Command::Scopes(args) => {
                    handlers::handle_scopes(&mut self.server, req, &args)?;
                }

                Command::Variables(args) => {
                    handlers::handle_variables(
                        &mut self.server,
                        &self.session,
                        req,
                        &args,
                    )?;
                }

                Command::Continue(_) => {
                    handlers::handle_continue(
                        &mut self.server,
                        &mut self.session,
                        &self.registry,
                        &self.operators,
                        req,
                    )?;
                }

                Command::Next(_) => {
                    handlers::handle_next(
                        &mut self.server,
                        &mut self.session,
                        &self.registry,
                        &self.operators,
                        req,
                    )?;
                }

                Command::StepIn(_) => {
                    handlers::handle_step_in(
                        &mut self.server,
                        &mut self.session,
                        &self.registry,
                        &self.operators,
                        req,
                    )?;
                }

                Command::StepOut(_) => {
                    handlers::handle_step_out(
                        &mut self.server,
                        &mut self.session,
                        &self.registry,
                        &self.operators,
                        req,
                    )?;
                }

                Command::Disconnect(_) => {
                    handlers::handle_disconnect(&mut self.server, &mut self.session, req)?;
                    return Ok(());
                }

                _ => {
                    // Unknown command - send error response
                    let resp = req.error("Unsupported command");
                    self.server.respond(resp)?;
                }
            }
        }
    }
}

impl Default for DapServer {
    fn default() -> Self {
        Self::new()
    }
}

/// Error type for DAP server operations.
#[derive(Debug)]
pub enum DapError {
    Io(io::Error),
    Server(dap::errors::ServerError),
}

impl std::fmt::Display for DapError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DapError::Io(e) => write!(f, "IO error: {}", e),
            DapError::Server(e) => write!(f, "Server error: {:?}", e),
        }
    }
}

impl std::error::Error for DapError {}

impl From<io::Error> for DapError {
    fn from(e: io::Error) -> Self {
        DapError::Io(e)
    }
}

impl From<dap::errors::ServerError> for DapError {
    fn from(e: dap::errors::ServerError) -> Self {
        DapError::Server(e)
    }
}

/// Run the DAP server on stdio.
pub fn run() -> io::Result<()> {
    let mut server = DapServer::new();
    server.run().map_err(io::Error::other)
}
