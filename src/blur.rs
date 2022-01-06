use raw_window_handle::HasRawWindowHandle;
use crate::platform;
use crate::Error;

/// Enables blurring of the background for the given window.
pub fn set_blur<H: HasRawWindowHandle>(window: &H, kind: Blur) -> Result<(), Error> {
	#[cfg(windows)]
	return match platform::windows::set_blur(window, kind) {
		Ok(_) => Ok(()),
		Err(platform::windows::Error::BlurNotSupported) => Err(Error::NotSupported),
		Err(what) => Err(Error::NotAvailable(format!("{}", what)))
	};

	#[cfg(not(any(
		windows
	)))]
	Err(Error::NotSupported)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Blur {
	Quality,
	Performance,
	Disabled,
}