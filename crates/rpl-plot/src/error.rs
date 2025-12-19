//! Error types for plot operations.

use std::fmt;

/// Errors that can occur when decoding plot data.
#[derive(Debug, Clone, PartialEq)]
pub enum DecodeError {
    /// Invalid or missing magic header (not a Plot blob).
    InvalidMagic,
    /// Unexpected end of data while decoding.
    UnexpectedEnd,
    /// Unknown command byte encountered.
    InvalidCommand(u8),
    /// Invalid UTF-8 in string data.
    InvalidString,
    /// Malformed number encoding.
    InvalidNumber,
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecodeError::InvalidMagic => write!(f, "not a Plot blob (invalid magic header)"),
            DecodeError::UnexpectedEnd => write!(f, "unexpected end of plot data"),
            DecodeError::InvalidCommand(cmd) => write!(f, "invalid command byte: 0x{:02X}", cmd),
            DecodeError::InvalidString => write!(f, "invalid string encoding"),
            DecodeError::InvalidNumber => write!(f, "invalid number encoding"),
        }
    }
}

impl std::error::Error for DecodeError {}

/// Errors that can occur when building plots.
#[derive(Debug, Clone, PartialEq)]
pub enum PlotError {
    /// Attempted operation without BEGINPLOT.
    NoPlotInProgress,
    /// Called BEGINPLOT while already building a plot.
    PlotAlreadyInProgress,
    /// Stack underflow during execution.
    StackUnderflow,
    /// Invalid argument to a command.
    InvalidArgument(String),
}

impl fmt::Display for PlotError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlotError::NoPlotInProgress => {
                write!(f, "no plot in progress; use BEGINPLOT first")
            }
            PlotError::PlotAlreadyInProgress => {
                write!(f, "plot already in progress; use ENDPLOT first")
            }
            PlotError::StackUnderflow => write!(f, "stack underflow"),
            PlotError::InvalidArgument(msg) => write!(f, "invalid argument: {}", msg),
        }
    }
}

impl std::error::Error for PlotError {}
