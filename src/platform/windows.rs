use raw_window_handle::{HasRawWindowHandle, RawWindowHandle, Win32Handle};
use winapi::shared::minwindef::{BOOL, DWORD};
use winapi::shared::ntdef::{NTSTATUS, PVOID, ULONG, WCHAR};
use winapi::shared::windef::{HWINEVENTHOOK, HWND};
use winapi::shared::winerror::S_OK;
use winapi::um::dwmapi::DwmExtendFrameIntoClientArea;
use winapi::um::errhandlingapi::GetLastError;
use winapi::um::libloaderapi::{FreeLibrary, GetProcAddress, LoadLibraryA};
use winapi::um::uxtheme::MARGINS;
use cstr::cstr;
use winapi::um::winbase::{FORMAT_MESSAGE_ALLOCATE_BUFFER, FORMAT_MESSAGE_FROM_SYSTEM, FORMAT_MESSAGE_IGNORE_INSERTS, FormatMessageW, LocalFree};
use winapi::um::winnt::{LANG_NEUTRAL, LONG, LPWSTR, MAKELANGID, SUBLANG_NEUTRAL};
use crate::Blur;
use std::collections::HashMap;
use std::sync::Mutex;
use winapi::um::processthreadsapi::GetCurrentProcessId;
use winapi::um::winuser::{CHILDID_SELF, EVENT_MAX, EVENT_MIN, EVENT_OBJECT_DESTROY, EVENT_SYSTEM_MOVESIZEEND, EVENT_SYSTEM_MOVESIZESTART, OBJID_WINDOW, SetWinEventHook};

/// Build number for Windows 10 1805.
const WINDOWS_10_1805: DWORD = 18362;

/// Build number for Windows 10 1903.
const WINDOWS_10_1903: DWORD = 18362;

/// Type for a pointer to the `SetWindowCompositionAttribute` function, found in
/// `user32.dll`. This function is responsible for requesting special window
/// composition effects, such as background blur and acrylic.
type __SetWindowCompositionAttribute =
	unsafe extern "system" fn(
		HWND,
		*mut WindowCompositionAttributeData) -> BOOL;

/// Type for a pointer to the `RtlGetVersion` function, found in `ntdll.dll`.
/// This function returns a number corresponding to the operating system
/// version, enabling us to select an appropriate blur method.
type __RtlGetVersion =
	unsafe extern "system" fn(*mut OperatingSystemVersionInfoWide) -> NTSTATUS;

lazy_static::lazy_static! {
	/// Memory used to keep track of the windows we've managing.
	static ref MEMORY: Mutex<HashMap<usize, WindowState>> = Default::default();

	/// Memory used to keep track of whether the event hook has been installed.
	static ref EVENT_HOOK_INSTALLED: Mutex<usize> = Default::default();

	/// The version of Windows the program is running under, if available.
	static ref VERSION: Option<OperatingSystemVersionInfoWide> = unsafe {
		let byte_size = std::mem::size_of::<OperatingSystemVersionInfoWide>();
		let byte_size = ULONG::try_from(byte_size)
			.expect("The size of an OperatingSystemVersionInfoWide is larger \
				than what can fit in a ULONG. This structure has a very \
				well-defined size in Windows and in this library, and it \
				should never naturally end up as bigger than a ULONG. This can \
				either point towards a broken build environment or a compiler \
				bug.");

		let mut version = OperatingSystemVersionInfoWide {
			byte_size,
			major_version: 0,
			minor_version: 0,
			build_number: 0,
			platform_id: 0,
			service_pack: [0; 128]
		};

		let library = LoadLibraryA(cstr!("ntdll.dll").as_ptr());
		if library.is_null() {
			/* Ntdll.dll is not available in this environment. This may happen
			 * in sandboxed environments such as UWP. Version information won't
			 * be available. */
			log::warn!(
				"Ntdll.dll is not available. Selected blur method may be \
				 wrong.");
			return None
		}

		let address = GetProcAddress(
			library,
			cstr!("RtlGetVersion").as_ptr());
		if address.is_null() {
			/* RtlGetVersion is not available in this environment. This may
			 * happen in a future version of Windows the library is not aware
			 * of. Version information won't be available. */
			log::warn!(
				"RtlGetVersion is not available. Selected blur method may be \
				 wrong.");
			return None
		}

		let result = {
			let function = std::mem
				::transmute
				::<_, __RtlGetVersion>(address);
			function(&mut version as *mut _)
		};
		if result != 0 {
			/* RtlGetVersion is an infallible function and, yet, it's returned
			 * an invalid value. This may be an indication of a bug or undefined
			 * behavior. */
			log::error!(
				"RtlGetVersion returned {} when it should have been an \
				 infallible function, whose only valid return value is \
				 STATUS_SUCCESS. This may be indicative of a bug.",
				result);
			return None
		}

		FreeLibrary(library);
		Some(version)
	};
}

pub fn set_blur<H: HasRawWindowHandle>(window: &H, kind: Blur) -> Result<(), Error> {
	let win7 = match kind {
		Blur::Disabled => AccentPolicyState::Disabled,
		_ => AccentPolicyState::BlurBehind,
	};
	let win10 = match kind {
		Blur::Disabled => AccentPolicyState::Disabled,
		Blur::Quality => AccentPolicyState::AcrylicBlurBehind,
		Blur::Performance => AccentPolicyState::BlurBehind
	};

	match window.raw_window_handle() {
		RawWindowHandle::Win32(Win32Handle { hwnd, .. }) => unsafe {
			let policy = match *VERSION {
				Some(OperatingSystemVersionInfoWide {
					major_version,
					build_number,
					.. }) => {

					if major_version < 6 {
						/* We're running in an environment that's at most as
						 * recent as Windows XP. Composition is entirely
						 * unsupported here. */
						return Err(Error::BlurNotSupported)
					} else if major_version < 10 {
						/* We're running in some flavor of Windows 7 or 8.
						 *
						 * We can only rely on aero blur being supported, but it
						 * looks good enough where it's meant to be seen. */
						AccentPolicy {
							state: win7,
							accent_flags: 0,
							gradient_color: 0,
							animation_id: 0
						}
					} else {
						if build_number < WINDOWS_10_1805 {
							/* We're running in a pre-1809 version of Windows
							 * 10. Acrylic blur is not available. */
							AccentPolicy {
								state: win7,
								accent_flags: 0,
								gradient_color: 0,
								animation_id: 0
							}
						} else {
							/* We're running modern Windows. Acrylic blur should
							 * be fully available and functional here.
							 *
							 * There's one caveat about this accent policy,
							 * though. That in Windows 10 1903 and later,
							 * generating too many events in a window with this
							 * effect enabled may lead to noticeable
							 * sluggishness. */
							AccentPolicy {
								state: win10,
								accent_flags: 0,
								gradient_color: 0x00101010,
								animation_id: 0
							}
						}
					}
				},
				None =>
					/* We can't assume anything, so better to give up now than
					 * risk messing up with someone else's window in a possibly
					 * unpredictable way. */
					return Err(Error::BlurNotSupported)
			};

			let mut margins = MARGINS {
				cxLeftWidth: 0,
				cxRightWidth: 0,
				cyTopHeight: 1,
				cyBottomHeight: 0
			};

			let result = DwmExtendFrameIntoClientArea(
				hwnd as *mut _,
				&mut margins as *mut _);
			if result != S_OK {
				return Err(Error::FailedInvocation {
					call: "DwmExtendFrameIntoClientArea",
					description: "extend window frame into the client area",
					error: WinError(result as DWORD)
				})
			}

			/* Save the current policy for this window. */
			let mut old = None;
			let mut memory = MEMORY.lock().unwrap();
			let state = memory
				.entry(hwnd as _)
				.and_modify(|current| {
					old = Some(current.accent);
					current.accent = policy;
				})
				.or_insert(WindowState {
					transform_hold: false,
					accent: policy
				});

			if !state.transform_hold {
				if let Some(current) = old {
					match (current.state, policy.state) {
						(AccentPolicyState::AcrylicBlurBehind, target)
							if target != AccentPolicyState::Disabled =>

							transition(hwnd as _, AccentPolicy {
								state: AccentPolicyState::Disabled,
								accent_flags: 0,
								gradient_color: 0,
								animation_id: 0
							}),
						(source, AccentPolicyState::AcrylicBlurBehind)
							if source != AccentPolicyState::Disabled =>

							transition(hwnd as _, AccentPolicy {
								state: AccentPolicyState::Disabled,
								accent_flags: 0,
								gradient_color: 0,
								animation_id: 0
							}),
						_ => Ok(())
					}?;
				}
				transition(hwnd as _, policy)?;
			}

			/* Handle installing the window event hook. */
			let mut installed = EVENT_HOOK_INSTALLED.lock().unwrap();
			if *installed == 0 {
				*installed = SetWinEventHook(
					EVENT_MIN,
					EVENT_MAX,
					std::ptr::null_mut(),
					Some(handle_window_events),
					GetCurrentProcessId(),
					0,
					0) as usize;
				if *installed == 0 {
					return Err(Error::FailedInvocation {
						call: "SetWinEventHook",
						description: "register hook for state upkeep",
						error: WinError(GetLastError())
					})
				}
			}

			Ok(())
		},
		RawWindowHandle::WinRt(_) =>
			/* Enabling the acrylic or micha effects in UWP is currently not
			 * supported when it really should be. Supporting it requires that
			 * we have access to Windows.UI.Composition, which will also require
			 * pulling in the `windows` crate.
			 *
			 * TODO: Add support for enabling blur for UWP windows through Windows.UI.Composition.
			 */
			Err(Error::BlurNotSupported),
		_ =>
			/* Windows supports no more handle types. */
			Err(Error::BlurNotSupported)
	}
}

/// Structure keeping track of the state of the windows we manage.
struct WindowState {
	/// Whether transitions should be put on hold.
	transform_hold: bool,
	/// What the intended accent policy state of the window is.
	accent: AccentPolicy
}

/// Actually transitions the window into the given accent policy state.
unsafe fn transition(hwnd: HWND, mut policy: AccentPolicy) -> Result<(), Error> {
	let mut data = WindowCompositionAttributeData {
		attribute: WindowCompositionAttribute::AccentPolicy,
		data: &mut policy as *mut AccentPolicy as *mut _,
		data_size: std::mem::size_of::<AccentPolicy>() as _
	};

	let lib = LoadLibraryA(cstr!("user32.dll").as_ptr());
	if lib.is_null() {
		return Err(Error::MissingFunctionality {
			name: "user32.dll",
			error: WinError(GetLastError())
		})
	}

	let address_set = GetProcAddress(
		lib,
		cstr!("SetWindowCompositionAttribute").as_ptr());
	if address_set.is_null() {
		return Err(Error::MissingFunctionality {
			name: "user32.dll\\SetWindowCompositionAttribute",
			error: WinError(GetLastError())
		})
	}

	let result = {
		let address_set = std::mem
		::transmute
			::<_, __SetWindowCompositionAttribute>(address_set);
		address_set(
			hwnd as *mut _,
			&mut data as *mut _)
	};
	if result == 0 {
		return Err(Error::FailedInvocation {
			call: "SetWindowCompositionAttribute",
			description: "set the composition attribute of the \
						window",
			error: WinError(GetLastError())
		})
	}

	FreeLibrary(lib);
	Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
	/// This error is triggered if this module doesn't know how to enable blur
	/// for the window type that was passed to it.
	#[error("Blur is not supported for this window type")]
	BlurNotSupported,
	/// This error is triggered if required functionality or one of its
	/// dependencies are not available in the system.
	#[error("Could not find \"{name}\": {error}")]
	MissingFunctionality {
		/// The name of the functionality that could not be found.
		name: &'static str,
		/// The error code given by `GetLastError()`.
		error: WinError
	},
	/// This error is triggered if required functionality and all of its
	/// dependencies are present, but in spite of that, the invocation of the
	/// functionality has failed.
	#[error("Could not {description}: call to \"{call}\" failed: {error}")]
	FailedInvocation {
		/// The name of the functionality whose invocation failed.
		call: &'static str,
		/// The description of what was trying to be achieved.
		description: &'static str,
		/// The error code given by `GetLastError()`.
		error: WinError
	}
}

/// Handles events for the windows we control.
///
/// This function is responsible for upkeep.
unsafe extern "system" fn handle_window_events(
	_this: HWINEVENTHOOK,
	event: DWORD,
	hwnd: HWND,
	object_id: LONG,
	child_id: LONG,
	_is_event_thread: DWORD,
	_event_time: DWORD) {

	if object_id != OBJID_WINDOW || child_id != CHILDID_SELF {
		/* We don't handle events that haven't been generated by windows
		 * themselves. This lets us avoid triggering actions wrongly on handles,
		 * by only invalidating handles once the windows have been destroyed. */
		return
	}

	match event {
		EVENT_OBJECT_DESTROY => {
			let mut memory = MEMORY.lock().unwrap();
			memory.remove(&(hwnd as _));
		},
		EVENT_SYSTEM_MOVESIZESTART => {
			let mut memory = MEMORY.lock().unwrap();
			let state = match memory.get_mut(&(hwnd as _)) {
				Some(state) => state,
				None => return
			};

			/* Some versions of Windows have trouble transforming windows while
			 * the acrylic blur effect enabled. Here, we work around that by
			 * transitioning the window into a cheaper and worse looking blur
			 * type, until the transformation is done. */
			let cheap_transform_required =
				VERSION.as_ref().map(|info| {
					info.major_version >= 10 && info.build_number >= WINDOWS_10_1903
				}).unwrap_or(false)
				&& state.accent.state == AccentPolicyState::AcrylicBlurBehind;

			if cheap_transform_required {
				state.transform_hold = true;
				let _ = transition(hwnd, AccentPolicy {
					state: AccentPolicyState::Disabled,
					accent_flags: 0,
					gradient_color: 0,
					animation_id: 0
				});
				let _ = transition(hwnd, AccentPolicy {
					state: AccentPolicyState::BlurBehind,
					accent_flags: 0,
					gradient_color: 0,
					animation_id: 0
				});
			}
		},
		EVENT_SYSTEM_MOVESIZEEND => {
			let mut memory = MEMORY.lock().unwrap();
			let state = match memory.get_mut(&(hwnd as _)) {
				Some(state) => state,
				None => return
			};

			/* Now, we transform the window to whatever state it has to be in. */
			if state.transform_hold {
				let _ = transition(hwnd, state.accent);
				state.transform_hold = false;
			}
		},
		_ => {}
	}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct WinError(DWORD);
impl WinError {
	/// Retrieve the message for this error message from Windows, if available.
	pub fn message(&self) -> Option<String> {
		let mut buffer: LPWSTR = std::ptr::null_mut();
		let size = unsafe {
			FormatMessageW(
				FORMAT_MESSAGE_ALLOCATE_BUFFER
					| FORMAT_MESSAGE_FROM_SYSTEM
					| FORMAT_MESSAGE_IGNORE_INSERTS,
				std::ptr::null_mut(),
				self.0,
				MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL) as DWORD,
				&mut buffer as *mut LPWSTR as *mut _,
				0,
				std::ptr::null_mut())
		};
		if size == 0 {
			/* No message. */
			return None
		}

		let slice = unsafe {
			std::slice::from_raw_parts(buffer, size as usize)
		};
		let message = std::char::decode_utf16(slice.iter().cloned())
			.map(|c| c.unwrap_or('_'))
			.collect::<String>()
			.trim()
			.to_string();

		/* Free the buffer FormatMessageW allocated for us. */
		unsafe {
			LocalFree(buffer as *mut _);
		}

		Some(message)
	}
}
impl std::fmt::Display for WinError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self.message() {
			Some(message) =>
				write!(f, "{} (0x{:08x})", message, self.0),
			None =>
				write!(f, "0x{:08x}", self.0),
		}
	}
}
impl std::error::Error for WinError {}

/// The operating system version information.
#[repr(C)]
struct OperatingSystemVersionInfoWide {
	/// The size of this structure.
	///
	/// This field must be set to the size of the structure, in bytes, so that
	/// Windows may recognize its type and fill it correctly. Setting the value
	/// to anything other than that will result in undefined behavior.
	///
	/// It's important to emphasize that this value must be correct even before
	/// it's first passed on to any Windows functionality, even if it appears to
	/// be write-only.
	byte_size: ULONG,
	/// The major version number of the operating system.
	///
	/// The meaning of this value may not have a direct correlation with the
	/// how the release of Windows being run is called, and should be instead
	/// derived, in conjunction with the value of [the minor version], from the
	/// information on this [table].
	///
	/// [the minor version]: Self::minor_version
	/// [table]: https://docs.microsoft.com/en-us/windows-hardware/drivers/ddi/wdm/ns-wdm-_osversioninfoexw#remarks
	major_version: ULONG,
	/// The major version number of the operating system.
	///
	/// The meaning of this value may not have a direct correlation with the
	/// how the release of Windows being run is called, and should be instead
	/// derived, in conjunction with the value of [the major version], from the
	/// information on this [table].
	///
	/// [the major version]: Self::major_version
	/// [table]: https://docs.microsoft.com/en-us/windows-hardware/drivers/ddi/wdm/ns-wdm-_osversioninfoexw#remarks
	minor_version: ULONG,
	/// The build number of the operating system.
	///
	/// In some versions of Windows, notably 10 and 11, the major and minor
	/// versions may not be enough to determine available functionality, and,
	/// thus, this value has to also be used.
	build_number: ULONG,
	/// The operating system platform.
	platform_id: ULONG,
	/// The service-pack version string.
	///
	/// If no service pack is installed, this value will not be changed by
	/// Windows query functionality. This imposes the need to zero-initialize
	/// this field if one wishes to access it later on.
	service_pack: [WCHAR; 128],
}

/// The structure informing the composition attributes of the window.
///
/// This structure has the same layout and size as the undocumented
/// `WINCOMPATTRDATA` structure, passed on to the
/// `SetWindowCompositionAttribute` function. It may be used in any version of
/// Windows greater than Vista.
#[repr(C)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct WindowCompositionAttributeData {
	/// The number of the attribute which is intended to be changed or queried.
	attribute: WindowCompositionAttribute,
	/// A pointer to the structure containing the data for the attribute.
	data: PVOID,
	/// The size, in bytes, of the attribute data structure.
	data_size: ULONG
}

/// The window composition attributes.
///
/// This structure defines the identification numbers of the known windows
/// composition attributes, which may be changed or queried through use of the
/// `WindowCompositionAttribute` family of functions.
#[repr(i32)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum WindowCompositionAttribute {
	/// The accent policy of the window.
	///
	/// This attribute tells the compositor, somewhat counter-intuitively, the
	/// way the background layer of the window should be treated. This treatment
	/// of the background may be used, for instance, to achieve transparency or
	/// a quasi-translucency effect through blurring.
	AccentPolicy = 19
}

/// The data for the accent policy attribute.
///
/// This structure defines the fields expected by the [accent policy attribute],
/// meaning all accesses to which must be done through it. Failure to uphold
/// this requirement may result in undefined behavior.
///
/// [accent policy attribute]: WindowCompositionAttribute::AccentPolicy
#[repr(C)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct AccentPolicy {
	/// The state of the accent policy.
	state: AccentPolicyState,
	/// Extra flags passed on to the accent policy.
	///
	/// The use of this field is contextual and its exact meaning, as well as
	/// what values are valid for use in it, is determined by the state value.
	accent_flags: i32,
	/// The color of the gradient effect.
	///
	/// The use of this field is contextual and its exact meaning, as well as
	/// what values are valid for use in it, is determined by the state value.
	gradient_color: i32,
	/// The identification number of the transition animation.
	///
	/// The use of this field is contextual and its exact meaning, as well as
	/// what values are valid for use in it, is determined by the state value.
	animation_id: i32,
}

/// The states the accent policy may assume.
#[repr(i32)]
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum AccentPolicyState {
	/// Disable accenting of the window.
	///
	/// This will instruct the compositor to leave the window as-is when drawing
	/// it on to the desktop. This is the default state for most windows and
	/// should be enough for most purposes, as it still allows for
	/// semitransparent windows.
	///
	/// All other fields of the [accent policy structure] must be set to zero
	/// when this state is used.
	///
	/// [accent policy structure]: AccentPolicy
	Disabled = 0,
	/// Draw a color gradient on the background layer of the window.
	Gradient = 1,
	/// Draw a semitransparent color gradient on the background layer of the window.
	TransparentGradient = 2,
	/// Blur what's behind the window.
	///
	/// This state enables the variation of the blur effect that's most commonly
	/// known as Aero Glass for its use in the Windows 7 Aero theme. Keep in
	/// mind that the effect does not change in appearance if used in more
	/// recent versions of Windows and, thus, it may look out of place if used
	/// there.
	///
	/// All other fields of the [accent policy structure] must be set to zero
	/// when this state is used.
	///
	/// [accent policy structure]: AccentPolicy
	BlurBehind = 3,
	/// Blur what's behind the window.
	///
	/// This state enables the variation of the blur effect that's most commonly
	/// knows as Acrylic for its use in the Windows 10 visual style of the same
	/// name. Keep in mind that the effect is not available in releases before
	/// Windows 10 and even in some Windows 10 builds.
	///
	/// The [`gradient_color`] field must be set to an `RGBA_8888`-encoded color
	/// value, which will be used by Windows to tint the acrylic effect. All
	/// other fields of the [accent policy structure] must be set to zero when
	/// this state is used.
	///
	/// [`gradient_color`]: AccentPolicy::gradient_color
	/// [accent policy structure]: AccentPolicy
	AcrylicBlurBehind = 4,
}
