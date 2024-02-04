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

use std::path::PathBuf;

// Build and link against a PCRE2 library with the given code unit width,
// which should be "8" or "32".
fn build_1_pcre2_lib(code_unit_width: &str) {
    let target = std::env::var("TARGET").unwrap();
    let upstream = PathBuf::from("upstream");
    // Set some config options. We mostly just use the default values. We do
    // this in lieu of patching config.h since it's easier.
    let mut builder = cc::Build::new();
    builder
        .define("PCRE2_CODE_UNIT_WIDTH", code_unit_width)
        .define("HAVE_STDLIB_H", "1")
        .define("HAVE_MEMMOVE", "1")
        .define("HAVE_CONFIG_H", "1")
        .define("PCRE2_STATIC", "1")
        .define("STDC_HEADERS", "1")
        .define(&format!("SUPPORT_PCRE2_{}", code_unit_width), "1")
        .define("SUPPORT_UNICODE", "1");
    if target.contains("windows") {
        builder.define("HAVE_WINDOWS_H", "1");
    }
    if feature_enabled("JIT") {
        enable_jit(&target, &mut builder);
    }

    builder.include(upstream.join("src")).include(upstream.join("include"));
    for result in std::fs::read_dir(upstream.join("src")).unwrap() {
        let dent = result.unwrap();
        let path = dent.path();
        if path.extension().map_or(true, |ext| ext != "c") {
            continue;
        }
        // Apparently PCRE2 doesn't want to compile these directly, but only as
        // included from pcre2_jit_compile.c.
        //
        // ... and also pcre2_ucptables.c, which is included by pcre2_tables.c.
        // This is despite NON-AUTOTOOLS-BUILD instructions saying that
        // pcre2_ucptables.c should be compiled directly.
        if path.ends_with("pcre2_jit_match.c")
            || path.ends_with("pcre2_jit_misc.c")
            || path.ends_with("pcre2_ucptables.c")
        {
            continue;
        }
        builder.file(path);
    }

    if std::env::var("PCRE2_SYS_DEBUG").unwrap_or(String::new()) == "1"
        || std::env::var("DEBUG").unwrap_or(String::new()) == "1"
    {
        builder.debug(true);
    }
    builder.compile(&format!("libpcre2-{}.a", code_unit_width));
}

fn main() {
    println!("cargo:rerun-if-env-changed=PCRE2_SYS_STATIC");

    let target = std::env::var("TARGET").unwrap();
    let do_utf32 = feature_enabled("UTF32");

    // Don't link to a system library if we want a static build.
    let want_static = pcre2_sys_static().unwrap_or(target.contains("musl"));
    if want_static || pkg_config::probe_library("libpcre2-8").is_err() {
        build_1_pcre2_lib("8");
    }
    if do_utf32
        && (want_static || pkg_config::probe_library("libpcre2-32").is_err())
    {
        build_1_pcre2_lib("32");
    }
}

// Return whether a given feature is enabled.
fn feature_enabled(feature: &str) -> bool {
    let env_var_name = format!("CARGO_FEATURE_{}", feature);
    match std::env::var(&env_var_name) {
        Ok(s) => s == "1",
        Err(_) => false,
    }
}

fn pcre2_sys_static() -> Option<bool> {
    match std::env::var("PCRE2_SYS_STATIC") {
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

// On `aarch64-apple-ios` clang fails with the following error.
//
//   Undefined symbols for architecture arm64:
//     "___clear_cache", referenced from:
//         _sljit_generate_code in libforeign.a(pcre2_jit_compile.o)
//   ld: symbol(s) not found for architecture arm64
//
// aarch64-apple-tvos         https://bugreports.qt.io/browse/QTBUG-62993?gerritReviewStatus=All
// aarch64-apple-darwin       https://github.com/Homebrew/homebrew-core/pull/57419
// x86_64-apple-ios           disabled for device–simulator consistency (not tested)
// x86_64-apple-tvos          disabled for device–simulator consistency (not tested)
// armv7-apple-ios            assumed equivalent to aarch64-apple-ios (not tested)
// armv7s-apple-ios           assumed equivalent to aarch64-apple-ios (not tested)
// i386-apple-ios             assumed equivalent to aarch64-apple-ios (not tested)
// x86_64-apple-ios-macabi    disabled out of caution (not tested) (needs attention)
// aarch64-linux-android      does not build
// armv7-linux-androideabi    does not build
// aarch64-unknown-linux-musl does not build
// *-*-*-musleabi*            does not build
//
// We may want to monitor developments on the `aarch64-apple-darwin` front as
// they may end up propagating to all `aarch64`-based targets and the `x86_64`
// equivalents.
fn enable_jit(target: &str, builder: &mut cc::Build) {
    if target.starts_with("aarch64-apple") {
        return;
    }
    if target == "aarch64-linux-android" {
        return;
    }
    if target == "armv7-linux-androideabi" {
        return;
    }
    if target == "aarch64-unknown-linux-musl" {
        return;
    }
    if target.contains("musleabi") {
        return;
    }
    if target.contains("apple-ios") || target.contains("apple-tvos") {
        return;
    }
    builder.define("SUPPORT_JIT", "1");
}
