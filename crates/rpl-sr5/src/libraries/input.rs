use std::sync::{Arc, Mutex};

use rpl::{
    Span,
    ir::{Branch, LibId},
    libs::{CommandInfo, ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor, LibraryInterface, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
};

use crate::hardware::Sr5Hardware;

/// SR5 Input library - BTNS, BTNP, BTNR
#[derive(Clone)]
pub struct Sr5InputLib {
    hardware: Arc<Mutex<Sr5Hardware>>,
}

/// SR5 Input library ID.
pub const SR5_INPUT_LIB: LibId = 201;

impl Sr5InputLib {
    const CMD_BTNS: u16 = 0;
    const CMD_BTNP: u16 = 1;
    const CMD_BTNR: u16 = 2;

    pub fn new(hardware: Arc<Mutex<Sr5Hardware>>) -> Self {
        Self { hardware }
    }
}

impl LibraryInterface for Sr5InputLib {
    fn id(&self) -> LibId {
        SR5_INPUT_LIB
    }

    fn name(&self) -> &'static str {
        "SR5Input"
    }

    fn commands(&self) -> Vec<CommandInfo> {
        vec![
            CommandInfo::with_effect("BTNS", SR5_INPUT_LIB, Self::CMD_BTNS, 0, 1),
            CommandInfo::with_effect("BTNP", SR5_INPUT_LIB, Self::CMD_BTNP, 0, 1),
            CommandInfo::with_effect("BTNR", SR5_INPUT_LIB, Self::CMD_BTNR, 0, 1),
        ]
    }
}

impl LibraryLowerer for Sr5InputLib {
    fn id(&self) -> LibId {
        SR5_INPUT_LIB
    }

    fn lower_composite(
        &self,
        _construct_id: u16,
        _branches: &[Branch],
        _span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError {
            message: "SR5 Input library has no composites".into(),
            span: None,
        })
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(SR5_INPUT_LIB, cmd);
        Ok(())
    }
}

impl LibraryExecutor for Sr5InputLib {
    fn id(&self) -> LibId {
        SR5_INPUT_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        let hw = match self.hardware.lock() {
            Ok(guard) => guard,
            Err(_) => return Err("Hardware lock poisoned".into()),
        };

        match ctx.cmd {
            Self::CMD_BTNS => {
                // BTNS: -- state (current button state)
                ctx.push(Value::Integer(hw.input.held() as i64))?;
                Ok(ExecuteAction::ok())
            }
            Self::CMD_BTNP => {
                // BTNP: -- state (newly pressed this frame)
                ctx.push(Value::Integer(hw.input.pressed() as i64))?;
                Ok(ExecuteAction::ok())
            }
            Self::CMD_BTNR => {
                // BTNR: -- state (newly released this frame)
                ctx.push(Value::Integer(hw.input.released() as i64))?;
                Ok(ExecuteAction::ok())
            }
            _ => Err(format!("Unknown input command: {}", ctx.cmd)),
        }
    }
}
