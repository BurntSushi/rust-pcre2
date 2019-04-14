use std::error;
use std::fmt;

use libc::c_int;
use pcre2_sys::*;

/// A PCRE2 error.
///
/// An error can occur during compilation or during matching. The kind of this
/// error indicates the type of operation being performed when the error
/// occurred.
#[derive(Clone)]
pub struct Error {
    kind: ErrorKind,
    code: c_int,
    offset: Option<usize>,
}

/// The kind of an error indicates the type of operation that was attempted
/// that resulted in an error.
///
/// This enum may expand over time.
#[derive(Clone, Debug)]
pub enum ErrorKind {
    /// An error occurred during compilation of a regex.
    Compile,
    /// An error occurred during JIT compilation of a regex.
    JIT,
    /// An error occurred while matching.
    Match,
    /// An error occurred while querying a compiled regex for info.
    Info,
    /// An error occurred while setting an option.
    Option,
    /// Hints that destructuring should not be exhaustive.
    ///
    /// This enum may grow additional variants, so this makes sure clients
    /// don't count on exhaustive matching. (Otherwise, adding a new variant
    /// could break existing code.)
    #[doc(hidden)]
    __Nonexhaustive,
}

impl Error {
    /// Create a new compilation error.
    pub(crate) fn compile(code: c_int, offset: usize) -> Error {
        Error {
            kind: ErrorKind::Compile,
            code: code,
            offset: Some(offset),
        }
    }

    /// Create a new JIT compilation error.
    pub(crate) fn jit(code: c_int) -> Error {
        Error {
            kind: ErrorKind::JIT,
            code: code,
            offset: None,
        }
    }

    /// Create a new matching error.
    pub(crate) fn matching(code: c_int) -> Error {
        Error {
            kind: ErrorKind::Match,
            code: code,
            offset: None,
        }
    }

    /// Create a new info error.
    pub(crate) fn info(code: c_int) -> Error {
        Error {
            kind: ErrorKind::Info,
            code: code,
            offset: None,
        }
    }

    /// Create a new option error.
    pub(crate) fn option(code: c_int) -> Error {
        Error {
            kind: ErrorKind::Option,
            code: code,
            offset: None,
        }
    }

    /// Return the kind of this error.
    ///
    /// The kind indicates the type of operation that was attempted which
    /// resulted in this error.
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    /// Return the raw underlying PCRE2 error code.
    ///
    /// This can be useful if one needs to determine exactly which error
    /// occurred, which can be done with case analysis over the constants
    /// exported in the `pcre2-sys` crate.
    pub fn code(&self) -> c_int {
        self.code
    }

    /// Return the underlying offset associated with this error, if one exists.
    ///
    /// The offset is typically only available for compile time errors, and
    /// is supposed to indicate the general position in the pattern where an
    /// error occurred.
    pub fn offset(&self) -> Option<usize> {
        self.offset
    }

    /// Returns the error message from PCRE2.
    fn error_message(&self) -> String {
        // PCRE2 docs say a buffer size of 120 bytes is enough, but we're
        // cautious and double it.
        let mut buf = [0u8; 240];
        let rc = unsafe {
            pcre2_get_error_message_8(self.code, buf.as_mut_ptr(), buf.len())
        };
        // Errors are only ever constructed from codes reported by PCRE2, so
        // our code should always be valid.
        assert!(rc != PCRE2_ERROR_BADDATA, "used an invalid error code");
        // PCRE2 docs claim 120 bytes is enough, and we use more, so...
        assert!(rc != PCRE2_ERROR_NOMEMORY, "buffer size too small");
        // Sanity check that we do indeed have a non-negative result. 0 is OK.
        assert!(rc >= 0, "expected non-negative but got {}", rc);
        String::from_utf8(buf[..rc as usize].to_vec()).expect("valid UTF-8")
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        "pcre2 error"
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = self.error_message();
        match self.kind {
            ErrorKind::Compile => {
                match self.offset {
                    None => {
                        write!(f, "PCRE2: error compiling pattern: {}", msg)
                    }
                    Some(offset) => {
                        write!(
                            f,
                            "PCRE2: error compiling pattern at offset {}: {}",
                            offset,
                            msg
                        )
                    }
                }
            }
            ErrorKind::JIT => {
                write!(f, "PCRE2: error JIT compiling pattern: {}", msg)
            }
            ErrorKind::Match => {
                write!(f, "PCRE2: error matching: {}", msg)
            }
            ErrorKind::Info => {
                write!(f, "PCRE2: error getting info: {}", msg)
            }
            ErrorKind::Option => {
                write!(f, "PCRE2: error setting option: {}", msg)
            }
            _ => unreachable!(),
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // We include the error message in the debug representation since
        // most humans probably don't have PCRE2 error codes memorized.
        f.debug_struct("Error")
            .field("kind", &self.kind)
            .field("code", &self.code)
            .field("offset", &self.offset)
            .field("message", &self.error_message())
            .finish()
    }
}
