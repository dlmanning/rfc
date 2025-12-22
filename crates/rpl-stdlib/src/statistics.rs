//! Statistics library.
//!
//! Provides statistical and random number functions:
//! - RAND - Generate a random number in [0, 1)
//! - RDZ - Seed the random number generator
//!
//! The RNG uses a Linear Congruential Generator (LCG) with the same
//! parameters as the classic MINSTD: a=48271, c=0, m=2^31-1.
//! The seed is stored in the VM directory as "__rng_seed".

use std::sync::OnceLock;

use rpl::{
    core::Span,
    interface::InterfaceSpec,
    ir::LibId,
    libs::{ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
};

/// Interface declaration for the Statistics library.
const INTERFACE: &str = include_str!("interfaces/statistics.rpli");

/// Get the runtime library (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid statistics interface"))
}

/// Statistics library ID.
pub const STATISTICS_LIB: LibId = 98;

/// Statistics library command IDs.
pub mod cmd {
    pub const RAND: u16 = 0;
    pub const RDZ: u16 = 1;
}

/// Name of the directory variable used to store RNG state.
const RNG_SEED_VAR: &str = "__rng_seed";

/// Default seed value (same as many HP calculators).
const DEFAULT_SEED: i64 = 999_999_999;

/// LCG multiplier (MINSTD).
const LCG_A: i64 = 48271;

/// LCG modulus (2^31 - 1, a Mersenne prime).
const LCG_M: i64 = 2_147_483_647;

/// Statistics library (implementation only).
#[derive(Clone, Copy)]
pub struct StatisticsLib;

impl LibraryLowerer for StatisticsLib {
    fn id(&self) -> LibId {
        STATISTICS_LIB
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(STATISTICS_LIB, cmd);
        Ok(())
    }
}

impl LibraryExecutor for StatisticsLib {
    fn id(&self) -> LibId {
        STATISTICS_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::RAND => {
                // Get current seed from directory, or use default
                let seed = ctx
                    .directory
                    .lookup(RNG_SEED_VAR)
                    .and_then(|v| match v {
                        Value::Integer(n) => Some(*n),
                        _ => None,
                    })
                    .unwrap_or(DEFAULT_SEED);

                // LCG: next = (a * seed) mod m
                let next = (LCG_A.wrapping_mul(seed)) % LCG_M;

                // Store new seed
                ctx.directory
                    .store(RNG_SEED_VAR.to_string(), Value::Integer(next));

                // Convert to [0, 1) range
                let result = (next as f64) / (LCG_M as f64);
                ctx.push(Value::Real(result))?;
                Ok(ExecuteAction::ok())
            }
            cmd::RDZ => {
                // Pop seed from stack
                let seed = match ctx.pop()? {
                    Value::Integer(n) => n,
                    Value::Real(r) => r as i64,
                    other => {
                        return Err(format!("RDZ: expected number, got {}", other.type_name()));
                    }
                };

                // Ensure seed is positive and within range
                let seed = seed.abs() % LCG_M;
                let seed = if seed == 0 { DEFAULT_SEED } else { seed };

                // Store seed in directory
                ctx.directory
                    .store(RNG_SEED_VAR.to_string(), Value::Integer(seed));
                Ok(ExecuteAction::ok())
            }
            _ => Err(format!("Unknown statistics command: {}", ctx.cmd)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn statistics_lib_id() {
        assert_eq!(interface().id(), 98);
    }

    #[test]
    fn statistics_lib_name() {
        assert_eq!(interface().name(), "Statistics");
    }

    #[test]
    fn rand_produces_values_in_range() {
        // Test that RAND produces values in [0, 1)
        let result = crate::eval("42 RDZ RAND RAND RAND").unwrap();
        assert_eq!(result.len(), 3);
        for val in result {
            if let Value::Real(r) = val {
                assert!((0.0..1.0).contains(&r), "RAND value {} out of range", r);
            } else {
                panic!("Expected Real, got {:?}", val);
            }
        }
    }

    #[test]
    fn rdz_seeds_deterministically() {
        // Same seed should produce same sequence
        let result1 = crate::eval("42 RDZ RAND RAND RAND").unwrap();
        let result2 = crate::eval("42 RDZ RAND RAND RAND").unwrap();
        assert_eq!(result1, result2);
    }

    #[test]
    fn different_seeds_produce_different_sequences() {
        let result1 = crate::eval("42 RDZ RAND").unwrap();
        let result2 = crate::eval("123 RDZ RAND").unwrap();
        assert_ne!(result1, result2);
    }
}
