#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

extern crate libc;

pub use bindings::*;

mod bindings;
#[cfg(test)]
mod tests;
