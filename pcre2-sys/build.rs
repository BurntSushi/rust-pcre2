// The build for pcre2-sys currently does roughly the following:
//
//   1. Use the PCRE2 system library as reported by pkg-config if it exists
//      and only if we don't explicitly want a static build.
//   2. Otherwise, statically build PCRE2 by hand.
//
// For step 1, we permit opting out of using the system library via either
// explicitly setting the PCRE2_SYS_STATIC environment variable or if we
// otherwise believe we want a static build (e.g., when building with MUSL).
//
// For step 2, we roughly follow the directions as laid out in
// pcre2/NON-AUTOTOOLS-BUILD. It's pretty straight-forward: copy a few files,
// set a few defines and then build it. We can get away with a pretty stripped
// down setup here since the PCRE2 build setup also handles various command
// line tools (like pcre2grep) and its test infrastructure, and we needn't
// concern ourselves with that.
//
// It is plausible that this build script will need to evolve to do better
// platform detection for the various PCRE2 settings, but this should work
// as-is on Windows, Linux and macOS.

extern crate cc;
extern crate pkg_config;

use std::process::Command;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

// Files that PCRE2 needs to compile.
const FILES: &'static [&'static str] = &[
   "pcre2_auto_possess.c",
   "pcre2_compile.c",
   "pcre2_config.c",
   "pcre2_context.c",
   "pcre2_convert.c",
   "pcre2_dfa_match.c",
   "pcre2_error.c",
   "pcre2_extuni.c",
   "pcre2_find_bracket.c",
   "pcre2_jit_compile.c",
   "pcre2_maketables.c",
   "pcre2_match.c",
   "pcre2_match_data.c",
   "pcre2_newline.c",
   "pcre2_ord2utf.c",
   "pcre2_pattern_info.c",
   "pcre2_serialize.c",
   "pcre2_string_utils.c",
   "pcre2_study.c",
   "pcre2_substitute.c",
   "pcre2_substring.c",
   "pcre2_tables.c",
   "pcre2_ucd.c",
   "pcre2_valid_utf.c",
   "pcre2_xclass.c",
];

fn main() {
    println!("cargo:rerun-if-env-changed=PCRE2_SYS_STATIC");

    let target = env::var("TARGET").unwrap();
    let out = PathBuf::from(env::var_os("OUT_DIR").unwrap());

    // Don't link to a system library if we want a static build.
    let want_static = pcre2_sys_static().unwrap_or(target.contains("musl"));
    if !want_static && pkg_config::probe_library("libpcre2-8").is_ok() {
        return;
    }

    // For a static build, make sure our PCRE2 submodule has been loaded.
    if has_git() && !Path::new("pcre2/.git").exists() {
        Command::new("git")
            .args(&["submodule", "update", "--init"])
            .status()
            .unwrap();
    }

    // Set some config options. We mostly just use the default values. We do
    // this in lieu of patching config.h since it's easier.
    let mut builder = cc::Build::new();
    builder
        .define("PCRE2_CODE_UNIT_WIDTH", "8")
        .define("HAVE_STDLIB_H", "1")
        .define("HAVE_MEMMOVE", "1")
        .define("HEAP_LIMIT", "20000000")
        .define("LINK_SIZE", "2")
        .define("MATCH_LIMIT", "10000000")
        .define("MATCH_LIMIT_DEPTH", "10000000")
        .define("MAX_NAME_COUNT", "10000")
        .define("MAX_NAME_SIZE", "32")
        .define("NEWLINE_DEFAULT", "2")
        .define("PARENS_NEST_LIMIT", "250")
        .define("PCRE2_STATIC", "1")
        .define("STDC_HEADERS", "1")
        .define("SUPPORT_JIT", "1")
        .define("SUPPORT_PCRE2_8", "1")
        .define("SUPPORT_UNICODE", "1");
    if target.contains("windows") {
        builder.define("HAVE_WINDOWS_H", "1");
    }

    // Copy PCRE2 headers. Typically, `./configure` would do this for us
    // automatically, but since we're compiling by hand, we do it ourselves.
    let include = out.join("include");
    fs::create_dir_all(&include).unwrap();
    fs::copy("pcre2/src/config.h.generic", include.join("config.h")).unwrap();
    fs::copy("pcre2/src/pcre2.h.generic", include.join("pcre2.h")).unwrap();

    // Same deal for chartables. Just use the default.
    let src = out.join("src");
    fs::create_dir_all(&src).unwrap();
    fs::copy(
        "pcre2/src/pcre2_chartables.c.dist",
        src.join("pcre2_chartables.c"),
    ).unwrap();

    // Build everything.
    builder
        .include("pcre2/src")
        .include(&include)
        .file(src.join("pcre2_chartables.c"));
    for file in FILES {
        builder.file(Path::new("pcre2/src").join(file));
    }

    if env::var("PCRE2_SYS_DEBUG").unwrap_or(String::new()) == "1" {
        builder.debug(true);
    }
    builder.compile("libpcre2.a");
}

fn has_git() -> bool {
    Command::new("git").arg("--help")
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

fn pcre2_sys_static() -> Option<bool> {
    match env::var("PCRE2_SYS_STATIC") {
        Err(_) => None,
        Ok(s) => {
            if s == "1" {
                Some(true)
            } else if s == "0" {
                Some(false)
            } else {
                None
            }
        }
    }
}
