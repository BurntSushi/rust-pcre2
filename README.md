pcre2
=====
A high level Rust wrapper library for [PCRE2](https://www.pcre.org/).

[![Linux build status](https://api.travis-ci.org/BurntSushi/rust-pcre2.png)](https://travis-ci.org/BurntSushi/rust-pcre2)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/BurntSushi/rust-pcre2?svg=true)](https://ci.appveyor.com/project/BurntSushi/rust-pcre2)
[![](http://meritbadge.herokuapp.com/pcre2)](https://crates.io/crates/pcre2)

Dual-licensed under MIT or the [UNLICENSE](http://unlicense.org).


### Documentation

https://docs.rs/pcre2


### Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
pcre2 = "0.1"
```

and this to your crate root:

```rust
extern crate pcre2;
```


### Notes

Currently, this is a fairly light layer around PCRE2 itself and does not even
come close to covering all of its functionality. There are no specific plans
in place to build out the wrapper further, but PRs for making more of PCRE2
available are welcome, although my bandwidth for maintenance is limited. If
you're interested in sharing this maintenance burden, please reach out.
