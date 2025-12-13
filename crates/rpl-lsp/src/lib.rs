//! LSP Server implementation.
//!
//! This module provides a Language Server Protocol implementation
//! for RPL that communicates over stdio.

mod handlers;
mod transport;

use std::io::{self, BufReader, BufWriter, Write};

pub use handlers::ServerState;
use handlers::*;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use transport::{read_message, write_message};

/// JSON-RPC request structure.
#[derive(Debug, Deserialize)]
struct Request {
    #[allow(dead_code)] // Required for JSON-RPC deserialization but not read
    jsonrpc: String,
    #[serde(default)]
    id: Option<Value>,
    method: String,
    #[serde(default)]
    params: Value,
}

/// JSON-RPC response structure.
#[derive(Debug, Serialize)]
struct Response {
    jsonrpc: String,
    id: Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<ResponseError>,
}

#[derive(Debug, Serialize)]
struct ResponseError {
    code: i32,
    message: String,
}

/// JSON-RPC notification structure.
#[derive(Debug, Serialize)]
struct Notification {
    jsonrpc: String,
    method: String,
    params: Value,
}

impl Response {
    fn success(id: Value, result: Value) -> Self {
        Self {
            jsonrpc: "2.0".into(),
            id,
            result: Some(result),
            error: None,
        }
    }

    fn error(id: Value, code: i32, message: String) -> Self {
        Self {
            jsonrpc: "2.0".into(),
            id,
            result: None,
            error: Some(ResponseError { code, message }),
        }
    }
}

impl Notification {
    fn new(method: &str, params: Value) -> Self {
        Self {
            jsonrpc: "2.0".into(),
            method: method.into(),
            params,
        }
    }
}

/// Run the LSP server on stdio.
pub fn run() -> io::Result<()> {
    let stdin = io::stdin();
    let stdout = io::stdout();

    let mut reader = BufReader::new(stdin.lock());
    let mut writer = BufWriter::new(stdout.lock());

    let mut state = ServerState::new();
    let mut initialized = false;
    let mut shutdown_requested = false;

    loop {
        let Some(message) = read_message(&mut reader)? else {
            // EOF
            break;
        };

        let request: Request = match serde_json::from_str(&message) {
            Ok(r) => r,
            Err(e) => {
                log::error!("Failed to parse request: {}", e);
                continue;
            }
        };

        // Handle the request
        let response = match request.method.as_str() {
            "initialize" => {
                let params: lsp_types::InitializeParams =
                    serde_json::from_value(request.params).unwrap_or_default();

                // Read initialization options
                if let Some(opts) = &params.initialization_options
                    && let Some(verbose) = opts.get("verboseHover").and_then(|v| v.as_bool())
                {
                    state.verbose_hover = verbose;
                }

                let result = handle_initialize(params);
                initialized = true;
                Some(Response::success(
                    request.id.unwrap_or(Value::Null),
                    serde_json::to_value(result).unwrap(),
                ))
            }

            "initialized" => {
                // Client acknowledges initialization - no response needed
                None
            }

            "shutdown" => {
                shutdown_requested = true;
                Some(Response::success(
                    request.id.unwrap_or(Value::Null),
                    Value::Null,
                ))
            }

            "exit" => {
                // Exit with code 0 if shutdown was requested, 1 otherwise
                std::process::exit(if shutdown_requested { 0 } else { 1 });
            }

            _ if !initialized => {
                // Reject requests before initialization
                Some(Response::error(
                    request.id.unwrap_or(Value::Null),
                    -32002,
                    "Server not initialized".into(),
                ))
            }

            "textDocument/didOpen" => {
                if let Ok(params) = serde_json::from_value(request.params) {
                    handle_did_open(&mut state, params);
                    // Publish diagnostics after opening
                    let uri = get_last_uri(&state);
                    if let Some(uri) = uri {
                        publish_diagnostics(&mut state, &uri, &mut writer)?;
                    }
                }
                None
            }

            "textDocument/didChange" => {
                if let Ok(params) =
                    serde_json::from_value::<lsp_types::DidChangeTextDocumentParams>(request.params)
                {
                    let uri = params.text_document.uri.to_string();
                    handle_did_change(&mut state, params);
                    // Publish diagnostics after change
                    publish_diagnostics(&mut state, &uri, &mut writer)?;
                }
                None
            }

            "textDocument/didClose" => {
                if let Ok(params) = serde_json::from_value(request.params) {
                    handle_did_close(&mut state, params);
                }
                None
            }

            "textDocument/completion" => {
                let result = serde_json::from_value(request.params)
                    .ok()
                    .and_then(|params| handle_completion(&mut state, params));
                Some(Response::success(
                    request.id.unwrap_or(Value::Null),
                    serde_json::to_value(result).unwrap_or(Value::Null),
                ))
            }

            "textDocument/hover" => {
                let result = serde_json::from_value(request.params)
                    .ok()
                    .and_then(|params| handle_hover(&mut state, params));
                Some(Response::success(
                    request.id.unwrap_or(Value::Null),
                    serde_json::to_value(result).unwrap_or(Value::Null),
                ))
            }

            "textDocument/definition" => {
                let result = serde_json::from_value(request.params)
                    .ok()
                    .and_then(|params| handle_definition(&mut state, params));
                Some(Response::success(
                    request.id.unwrap_or(Value::Null),
                    serde_json::to_value(result).unwrap_or(Value::Null),
                ))
            }

            "textDocument/references" => {
                let result = serde_json::from_value(request.params)
                    .ok()
                    .and_then(|params| handle_references(&mut state, params));
                Some(Response::success(
                    request.id.unwrap_or(Value::Null),
                    serde_json::to_value(result).unwrap_or(Value::Null),
                ))
            }

            "textDocument/semanticTokens/full" => {
                let result = serde_json::from_value(request.params)
                    .ok()
                    .and_then(|params| handle_semantic_tokens_full(&mut state, params));
                Some(Response::success(
                    request.id.unwrap_or(Value::Null),
                    serde_json::to_value(result).unwrap_or(Value::Null),
                ))
            }

            "textDocument/documentSymbol" => {
                let result = serde_json::from_value(request.params)
                    .ok()
                    .and_then(|params| handle_document_symbol(&mut state, params));
                Some(Response::success(
                    request.id.unwrap_or(Value::Null),
                    serde_json::to_value(result).unwrap_or(Value::Null),
                ))
            }

            method => {
                log::warn!("Unhandled method: {}", method);
                if request.id.is_some() {
                    // Only respond to requests, not notifications
                    Some(Response::error(
                        request.id.unwrap_or(Value::Null),
                        -32601,
                        format!("Method not found: {}", method),
                    ))
                } else {
                    None
                }
            }
        };

        // Send response if any
        if let Some(response) = response {
            let json = serde_json::to_string(&response)?;
            write_message(&mut writer, &json)?;
        }
    }

    Ok(())
}

/// Publish diagnostics for a document.
fn publish_diagnostics<W: Write>(
    state: &mut ServerState,
    uri: &str,
    writer: &mut W,
) -> io::Result<()> {
    let diagnostics = get_diagnostics(state, uri);

    let notification = Notification::new(
        "textDocument/publishDiagnostics",
        json!({
            "uri": uri,
            "diagnostics": diagnostics,
        }),
    );

    let json = serde_json::to_string(&notification)?;
    write_message(writer, &json)
}

/// Get the URI of the most recently added document.
fn get_last_uri(state: &ServerState) -> Option<String> {
    state.documents.keys().last().cloned()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn request_parsing() {
        let json = r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}"#;
        let req: Request = serde_json::from_str(json).unwrap();
        assert_eq!(req.method, "initialize");
        assert_eq!(req.id, Some(Value::Number(1.into())));
    }

    #[test]
    fn response_serialization() {
        let resp = Response::success(Value::Number(1.into()), json!({"capabilities":{}}));
        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"result\""));
        assert!(!json.contains("\"error\""));
    }

    #[test]
    fn error_response_serialization() {
        let resp = Response::error(Value::Number(1.into()), -32600, "Invalid request".into());
        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"error\""));
        assert!(json.contains("-32600"));
    }
}
