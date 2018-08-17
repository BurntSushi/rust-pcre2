extern crate cc;
extern crate pkg_config;

use std::env;
use std::path::Path;

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
    let target = env::var("TARGET").unwrap();
    // let host = env::var("HOST").unwrap();
    // let src = env::current_dir().unwrap();
    // let dst = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let windows = target.contains("windows");

    // pkg_config::probe_library("libpcre2-8").unwrap();
    let mut builder = cc::Build::new();
    builder
        .define("PCRE2_CODE_UNIT_WIDTH", "8")
        .define("HAVE_BCOPY_H", "1")
        .define("HAVE_INTTYPES_H", "1")
        .define("HAVE_MEMMOVE", "1")
        .define("HAVE_MEMORY_H", "1")
        .define("HAVE_STDINT_H", "1")
        .define("HAVE_STDLIB_H", "1")
        .define("HAVE_STRERROR", "1")
        .define("HAVE_STRINGS_H", "1")
        .define("HAVE_STRING_H", "1")
        .define("HEAP_LIMIT", "20000000")
        .define("LINK_SIZE", "2")
        .define("LT_OBJDIR", ".libs/")
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
    if windows {
        builder.define("HAVE_WINDOWS_H", "1");
    }

    builder
        .include("pcre-custom")
        .include("pcre/src");
    for file in FILES {
        builder.file(Path::new("pcre/src").join(file));
    }
    builder.file("pcre-custom/pcre2_chartables.c");
    builder.compile("pcre2");
}
