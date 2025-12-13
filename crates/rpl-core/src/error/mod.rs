mod code;
mod diagnostic;

pub use code::ErrorCode;
pub use diagnostic::{Diagnostic, DiagnosticBuilder, Severity, Suggestion};
// Note: DiagnosticRenderer is in rpl-source since it needs SourceFile
