//! SR5 UI library override - hardware-backed KEY, WAIT, KEYEVAL
//!
//! Overrides the stdlib UI library (76) with SR5 hardware implementation.

use std::{
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

use rpl::{
    Span,
    ir::{Branch, LibId},
    libs::{
        CommandInfo, ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor,
        LibraryInterface, LibraryLowerer,
    },
    lower::{LowerContext, LowerError},
    value::Value,
};

use crate::hardware::{Sr5Hardware, buttons};

/// SR5 UI library - overrides stdlib UI (76)
#[derive(Clone)]
pub struct Sr5UiLib {
    hardware: Arc<Mutex<Sr5Hardware>>,
}

/// UI library ID (same as stdlib to override it)
pub const UI_LIB: LibId = 76;

/// Button name mapping
const BUTTON_NAMES: &[(u16, &str)] = &[
    (buttons::A, "A"),
    (buttons::B, "B"),
    (buttons::X, "X"),
    (buttons::Y, "Y"),
    (buttons::L, "L"),
    (buttons::R, "R"),
    (buttons::START, "START"),
    (buttons::SELECT, "SELECT"),
    (buttons::UP, "UP"),
    (buttons::DOWN, "DOWN"),
    (buttons::LEFT, "LEFT"),
    (buttons::RIGHT, "RIGHT"),
    (buttons::ENTER, "ENTER"),
    (buttons::MODE, "MODE"),
    (buttons::VARS, "VARS"),
];

impl Sr5UiLib {
    const CMD_KEY: u16 = 0;
    const CMD_WAIT: u16 = 1;
    const CMD_KEYEVAL: u16 = 2;

    pub fn new(hardware: Arc<Mutex<Sr5Hardware>>) -> Self {
        Self { hardware }
    }

    /// Convert button name to bit flag
    fn name_to_button(name: &str) -> Option<u16> {
        let name_upper = name.to_uppercase();
        BUTTON_NAMES
            .iter()
            .find(|(_, n)| *n == name_upper)
            .map(|(bit, _)| *bit)
    }

    /// Get currently pressed button names
    fn get_pressed_keys(&self) -> Vec<String> {
        let state = match self.hardware.lock() {
            Ok(hw) => hw.input.held(),
            Err(_) => return vec![],
        };

        BUTTON_NAMES
            .iter()
            .filter(|(bit, _)| state & bit != 0)
            .map(|(_, name)| name.to_string())
            .collect()
    }

    /// Simulate a button press
    fn simulate_key(&self, name: &str) -> bool {
        if let Some(bit) = Self::name_to_button(name)
            && let Ok(mut hw) = self.hardware.lock()
        {
            // Set the button bit in current state
            let current = hw.input.current;
            hw.input.update(current | bit);
            return true;
        }
        false
    }

    /// Wait for a key press with timeout (returns key name or empty string)
    fn wait_for_key(&self, timeout_secs: f64) -> String {
        let start = Instant::now();
        let timeout = Duration::from_secs_f64(timeout_secs);

        // Get initial state to detect new presses
        let initial_state = match self.hardware.lock() {
            Ok(hw) => hw.input.held(),
            Err(_) => return String::new(),
        };

        loop {
            // Check if timeout expired
            if start.elapsed() >= timeout {
                return String::new();
            }

            // Check for new key presses
            let current_state = match self.hardware.lock() {
                Ok(hw) => hw.input.held(),
                Err(_) => return String::new(),
            };

            // Find newly pressed keys (not in initial state)
            let new_presses = current_state & !initial_state;
            if new_presses != 0 {
                // Return the first newly pressed key name
                for (bit, name) in BUTTON_NAMES {
                    if new_presses & bit != 0 {
                        return name.to_string();
                    }
                }
            }

            // Small sleep to avoid busy-waiting
            std::thread::sleep(Duration::from_millis(10));
        }
    }
}

impl LibraryInterface for Sr5UiLib {
    fn id(&self) -> LibId {
        UI_LIB
    }

    fn name(&self) -> &'static str {
        "UI"
    }

    fn commands(&self) -> Vec<CommandInfo> {
        vec![
            CommandInfo::with_effect("KEY", UI_LIB, Self::CMD_KEY, 0, 1), // variadic output
            CommandInfo::with_effect("WAIT", UI_LIB, Self::CMD_WAIT, 1, 1), // variadic output
            CommandInfo::with_effect("KEYEVAL", UI_LIB, Self::CMD_KEYEVAL, 1, 0),
        ]
    }
}

impl LibraryLowerer for Sr5UiLib {
    fn id(&self) -> LibId {
        UI_LIB
    }

    fn lower_composite(
        &self,
        _construct_id: u16,
        _branches: &[Branch],
        _span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError {
            message: "UI library has no composites".into(),
            span: None,
        })
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(UI_LIB, cmd);
        Ok(())
    }
}

impl LibraryExecutor for Sr5UiLib {
    fn id(&self) -> LibId {
        UI_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            Self::CMD_KEY => {
                // KEY: -> S1 S2 ... Sn N
                // Non-blocking, returns pressed key names + count
                let keys = self.get_pressed_keys();
                let count = keys.len() as i64;

                // Push key names first
                for key in keys {
                    ctx.push(Value::String(key.into()))?;
                }
                // Push count
                ctx.push(Value::Integer(count))?;

                Ok(ExecuteAction::ok())
            }
            Self::CMD_WAIT => {
                // WAIT: R ->
                // R > 0: wait R seconds, return nothing
                // R < 0: wait for key (timeout |R| secs), return key name or ""
                let timeout = match ctx.pop()? {
                    Value::Integer(i) => i as f64,
                    Value::Real(r) => r,
                    _ => return Err("WAIT: expected number".into()),
                };

                if timeout > 0.0 {
                    // Positive: just wait
                    std::thread::sleep(Duration::from_secs_f64(timeout));
                    Ok(ExecuteAction::ok())
                } else if timeout < 0.0 {
                    // Negative: wait for key with timeout
                    let key = self.wait_for_key(timeout.abs());
                    ctx.push(Value::String(key.into()))?;
                    Ok(ExecuteAction::ok())
                } else {
                    // Zero: return immediately with no key
                    ctx.push(Value::String("".into()))?;
                    Ok(ExecuteAction::ok())
                }
            }
            Self::CMD_KEYEVAL => {
                // KEYEVAL: S ->
                // Simulate pressing key named by string
                let key_name = match ctx.pop()? {
                    Value::String(s) => s.to_string(),
                    _ => return Err("KEYEVAL: expected string".into()),
                };

                if !self.simulate_key(&key_name) {
                    return Err(format!("KEYEVAL: unknown key '{}'", key_name));
                }

                Ok(ExecuteAction::ok())
            }
            _ => Err(format!("Unknown UI command: {}", ctx.cmd)),
        }
    }
}
