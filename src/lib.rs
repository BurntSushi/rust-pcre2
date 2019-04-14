/*!
This crate provides a safe high level Rust binding to
[PCRE2](https://www.pcre.org/).

The API of this crate attempts to correspond closely to the API of Rust's
[`regex`](https://docs.rs/regex)
crate. The API provided by this crate neither matches the full API of Rust's
regex crate nor does it expose the full functionality of PCRE2. Contributions
are welcome to improve this.
*/

#![deny(missing_docs)]

pub use crate::error::{Error, ErrorKind};
pub use crate::ffi::{is_jit_available, version};

/**
PCRE2 regular expressions for matching on arbitrary bytes.
*/
pub mod bytes;
mod error;
mod ffi;
