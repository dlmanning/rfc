mod kind;
mod registry;
mod signature;

pub use kind::OperatorKind;
pub use registry::OperatorRegistry;
pub use signature::{
    CoercionPath, CoercionRegistration, OpSignature, OperatorRegistration, Resolution,
};
