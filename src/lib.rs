/// Platform-specific code for handling windows.
mod platform;

/// Public interface for handling the blurring of window backgrounds.
mod blur;
pub use blur::*;

/// Errors types that may be triggered from WinEx functions.
#[derive(Debug, thiserror::Error)]
pub enum Error {
	/// This error indicates that the given window type in the current system
	/// does not support blur through this crate.
	#[error("feature is not supported for this window")]
	NotSupported,
	/// This error indicates that, while blur may be supported for the given
	/// window in the current system, it could not be enabled in the current
	/// environment.
	#[error("feature is not available for this window: {0}")]
	NotAvailable(String),
}

