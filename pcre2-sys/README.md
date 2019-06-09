pcre2-sys
=========
Bindings for [PCRE2](https://www.pcre.org/).

[![Linux build status](https://api.travis-ci.org/BurntSushi/rust-pcre2.png)](https://travis-ci.org/BurntSushi/rust-pcre2)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/BurntSushi/rust-pcre2?svg=true)](https://ci.appveyor.com/project/BurntSushi/rust-pcre2)
[![](http://meritbadge.herokuapp.com/pcre2-sys)](https://crates.io/crates/pcre2-sys)

Dual-licensed under MIT or the [UNLICENSE](http://unlicense.org).


### Documentation

https://docs.rs/pcre2-sys


### Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
pcre2-sys = "0.1"
```

and this to your crate root:

```rust
extern crate pcre2_sys;
```


### Notes

As a `-sys` crate, this exposes only the bindings to PCRE2 based on the header
file. The PCRE2 documentation itself should be consulted in order to use this
crate.

The bindings for this crate were generated for PCRE **10.31**. This crate
intends to track the current release of PCRE2.

The build script for this crate prefers dynamically linking with the host's
PCRE2 system library. If that isn't available or if static linking is desired,
then PCRE2 is built from source and statically linked.

Static linking will automatically happen for MUSL targets, but can be forced by
setting the `PCRE2_SYS_STATIC` environment variable to `1`. Similarly, if
`PCRE2_SYS_STATIC` is set to `0`, then static linking will be forcefully
disabled, even for MUSL targets.

Currently, this crate only supports `libpcre-8` where
`PCRE2_CODE_UNIT_WIDTH=8`.

This crate has been tested to work on Windows, Linux and macOS. Other platforms
may work, and PRs to support them are welcome.

If you're compiling this crate on Windows with the GNU toolchain, then you'll
need to make sure you have a compatible C compiler installed, such as the one
provided by the [mingw-w64](http://mingw-w64.org) project.

Set the `PCRE2_SYS_DEBUG` flag to forcefully enable debug symbols when doing a
static build, even when compiling in release mode.
