#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

pub use crate::bindings::*;

mod bindings;
#[cfg(test)]
mod tests;

// It is weird that this isn't caught by bindgen. Dunno why.
pub const PCRE2_UNSET: usize = ::std::usize::MAX;
