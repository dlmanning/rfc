//! RPL Debug Adapter Protocol server.

fn main() {
    if let Err(e) = rpl_dap::run() {
        eprintln!("DAP server error: {}", e);
        std::process::exit(1);
    }
}
