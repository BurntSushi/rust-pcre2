#![allow(dead_code, unused_imports, unused_variables)]

extern crate libc;
extern crate pcre2_sys;
extern crate thread_local;

pub use error::{Error, ErrorKind};

pub mod bytes;
mod error;
