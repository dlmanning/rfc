//! RPL Language Server Protocol server.

fn main() {
    if let Err(e) = rpl_lsp::run() {
        eprintln!("LSP server error: {}", e);
        std::process::exit(1);
    }
}
