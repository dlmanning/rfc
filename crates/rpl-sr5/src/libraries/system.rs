use std::sync::{Arc, Mutex};

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

use crate::hardware::Sr5Hardware;

/// SR5 System library - FRAME, TICKS, VSYNC
#[derive(Clone)]
pub struct Sr5SystemLib {
    hardware: Arc<Mutex<Sr5Hardware>>,
}

/// SR5 System library ID.
pub const SR5_SYSTEM_LIB: LibId = 202;

impl Sr5SystemLib {
    const CMD_FRAME: u16 = 0;
    const CMD_TICKS: u16 = 1;
    const CMD_VSYNC: u16 = 2;
    const CMD_SCREEN: u16 = 3;

    pub fn new(hardware: Arc<Mutex<Sr5Hardware>>) -> Self {
        Self { hardware }
    }
}

impl LibraryInterface for Sr5SystemLib {
    fn id(&self) -> LibId {
        SR5_SYSTEM_LIB
    }

    fn name(&self) -> &'static str {
        "SR5System"
    }

    fn commands(&self) -> Vec<CommandInfo> {
        vec![
            CommandInfo::with_effect("FRAME", SR5_SYSTEM_LIB, Self::CMD_FRAME, 0, 1),
            CommandInfo::with_effect("TICKS", SR5_SYSTEM_LIB, Self::CMD_TICKS, 0, 1),
            CommandInfo::with_effect("VSYNC", SR5_SYSTEM_LIB, Self::CMD_VSYNC, 0, 0),
            CommandInfo::with_effect("SCREEN", SR5_SYSTEM_LIB, Self::CMD_SCREEN, 2, 0),
        ]
    }
}

impl LibraryLowerer for Sr5SystemLib {
    fn id(&self) -> LibId {
        SR5_SYSTEM_LIB
    }

    fn lower_composite(
        &self,
        _construct_id: u16,
        _branches: &[Branch],
        _span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError {
            message: "SR5 System library has no composites".into(),
            span: None,
        })
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(SR5_SYSTEM_LIB, cmd);
        Ok(())
    }
}

impl LibraryExecutor for Sr5SystemLib {
    fn id(&self) -> LibId {
        SR5_SYSTEM_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        let mut hw = match self.hardware.lock() {
            Ok(guard) => guard,
            Err(_) => return Err("Hardware lock poisoned".into()),
        };

        match ctx.cmd {
            Self::CMD_FRAME => {
                // FRAME: -- count
                ctx.push(Value::Integer(hw.frame_count as i64))?;
                Ok(ExecuteAction::ok())
            }
            Self::CMD_TICKS => {
                // TICKS: -- ms
                ctx.push(Value::Integer(hw.ticks as i64))?;
                Ok(ExecuteAction::ok())
            }
            Self::CMD_VSYNC => {
                // VSYNC: -- (signal request to wait for vblank)
                hw.vsync_requested = true;
                Ok(ExecuteAction::ok())
            }
            Self::CMD_SCREEN => {
                // SCREEN: width height --
                let height = match ctx.pop() {
                    Ok(Value::Integer(v)) => v as u32,
                    Ok(Value::Real(v)) => v as u32,
                    Ok(other) => {
                        return Err(format!(
                            "SCREEN: expected number for height, got {:?}",
                            other
                        ));
                    }
                    Err(_) => return Err("SCREEN: stack underflow".into()),
                };
                let width = match ctx.pop() {
                    Ok(Value::Integer(v)) => v as u32,
                    Ok(Value::Real(v)) => v as u32,
                    Ok(other) => {
                        return Err(format!(
                            "SCREEN: expected number for width, got {:?}",
                            other
                        ));
                    }
                    Err(_) => return Err("SCREEN: stack underflow".into()),
                };

                // Validate reasonable bounds
                if !(128..=1920).contains(&width) || !(128..=1080).contains(&height) {
                    return Err("SCREEN: resolution must be 128-1920 x 128-1080".into());
                }

                hw.set_resolution(width, height)?;
                Ok(ExecuteAction::ok())
            }
            _ => Err(format!("Unknown system command: {}", ctx.cmd)),
        }
    }
}
