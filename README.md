pcre2
=====
A high level Rust wrapper library for [PCRE2](https://www.pcre.org/).

[![Build status](https://github.com/BurntSushi/rust-pcre2/workflows/ci/badge.svg)](https://github.com/BurntSushi/rust-pcre2/actions)
[![crates.io](https://img.shields.io/crates/v/pcre2.svg)](https://crates.io/crates/pcre2)

Dual-licensed under MIT or the [UNLICENSE](https://unlicense.org/).


### Documentation

https://docs.rs/pcre2


### Usage

Run `cargo add pcre2` to add this crate to your `Cargo.toml` file.


### Notes

Currently, this is a fairly light layer around PCRE2 itself and does not even
come close to covering all of its functionality. There are no specific plans
in place to build out the wrapper further, but PRs for making more of PCRE2
available are welcome, although my bandwidth for maintenance is limited. If
you're interested in sharing this maintenance burden, please reach out.
