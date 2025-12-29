//! UI library - keyboard input and waiting.
//!
//! Provides keyboard input commands:
//! - KEY: Non-blocking read of pressed keys
//! - WAIT: Blocking wait (time or keypress)
//! - KEYEVAL: Simulate a keypress
//!
//! Note: The stdlib implementation provides stub behavior.
//! Platform-specific runtimes (like SR5) should override with
//! hardware-backed implementations.

use std::sync::OnceLock;

use rpl::core::Span;
use rpl::interface::InterfaceSpec;
use rpl::{
    ir::LibId,
    libs::{ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
};

/// Interface declaration for the UI library.
const INTERFACE: &str = include_str!("interfaces/ui.rpli");

/// Get the interface specification (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid ui interface"))
}

/// UI library ID (matches newRPL).
pub const UI_LIB: LibId = 76;

/// UI library command IDs.
pub mod cmd {
    pub const KEY: u16 = 0;
    pub const WAIT: u16 = 1;
    pub const KEYEVAL: u16 = 2;
}

/// UI library (implementation).
#[derive(Clone, Copy)]
pub struct UiLib;

impl LibraryLowerer for UiLib {
    fn id(&self) -> LibId {
        UI_LIB
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        // All commands use library call
        ctx.output.emit_call_lib(UI_LIB, cmd);
        Ok(())
    }
}

impl LibraryExecutor for UiLib {
    fn id(&self) -> LibId {
        UI_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::KEY => {
                // Non-blocking key read - stub returns 0 (no keys pressed)
                ctx.push(Value::Integer(0))?;
                Ok(ExecuteAction::ok())
            }
            cmd::WAIT => {
                // Blocking wait
                let timeout = match ctx.pop()? {
                    Value::Integer(i) => i as f64,
                    Value::Real(r) => r,
                    _ => return Err("WAIT: expected number".into()),
                };

                if timeout > 0.0 {
                    // Positive: just wait, return nothing
                    // Skip sleep on WASM (no WASI clock support in IDE)
                    #[cfg(not(target_arch = "wasm32"))]
                    std::thread::sleep(std::time::Duration::from_secs_f64(timeout));
                    Ok(ExecuteAction::ok())
                } else {
                    // Negative: wait for keypress with timeout
                    // Stub implementation: return empty string immediately (no key)
                    ctx.push(Value::String("".into()))?;
                    Ok(ExecuteAction::ok())
                }
            }
            cmd::KEYEVAL => {
                // Simulate keypress - stub does nothing
                let _key_name = match ctx.pop()? {
                    Value::String(s) => s,
                    _ => return Err("KEYEVAL: expected string".into()),
                };
                // No-op in stub implementation
                Ok(ExecuteAction::ok())
            }
            _ => Err(format!("Unknown UI command: {}", ctx.cmd)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ui_lib_id() {
        assert_eq!(interface().id(), 76);
    }

    #[test]
    fn ui_lib_name() {
        assert_eq!(interface().name(), "UI");
    }
}
