.PHONY: all build test check clean setup

# Default: build everything
all: build

# Build everything (extension depends on WASM, which depends on Rust)
build: crates/rpl-ide/vscode/out/extension.js

# The extension output depends on the transpiled WASM
crates/rpl-ide/vscode/out/extension.js: crates/rpl-ide/vscode/src/rpl-ide/rpl_ide.js crates/rpl-ide/vscode/src/extension.ts
	cd crates/rpl-ide/vscode && npm run build:ext

# Transpiled JS depends on the WASM binary
crates/rpl-ide/vscode/src/rpl-ide/rpl_ide.js: crates/rpl-ide/vscode/wasm/rpl_ide.wasm
	cd crates/rpl-ide/vscode && npm run transpile

# WASM binary depends on Rust sources
WASM_DEPS := $(shell find crates -name '*.rs') $(shell find crates -name 'Cargo.toml')
crates/rpl-ide/vscode/wasm/rpl_ide.wasm: $(WASM_DEPS)
	cargo build -p rpl-ide --target wasm32-wasip2 --release
	cp target/wasm32-wasip2/release/rpl_ide.wasm crates/rpl-ide/vscode/wasm/

# Run tests
test:
	cargo test

# Type check
check:
	cargo check
	cd crates/rpl-ide/vscode && npm run check

# Clean everything
clean:
	cargo clean
	rm -rf crates/rpl-ide/vscode/out
	rm -rf crates/rpl-ide/vscode/src/rpl-ide
	rm -f crates/rpl-ide/vscode/wasm/rpl_ide.wasm

# First-time setup
setup:
	cd crates/rpl-ide/vscode && npm install
