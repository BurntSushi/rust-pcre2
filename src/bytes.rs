use crate::ffi::CodeUnitWidth8;
pub use crate::regex_impl::Captures as CapturesImpl;
pub use crate::regex_impl::Match as MatchImpl;

#[doc(inline)]
pub use crate::regex_impl::{
    Regex as RegexImpl, RegexBuilder as RegexBuilderImpl,
};

/// A compiled PCRE2 regular expression for matching bytes.
///
/// This regex is safe to use from multiple threads simultaneously. For top
/// performance, it is better to clone a new regex for each thread.
pub type Regex = RegexImpl<CodeUnitWidth8>;

/// A builder for configuring the compilation of a PCRE2 regex.
pub type RegexBuilder = RegexBuilderImpl<CodeUnitWidth8>;

/// Match represents a single match of a regex in a subject string.
///
/// The lifetime parameter `'s` refers to the lifetime of the matched portion
/// of the subject string.
pub type Match<'s> = MatchImpl<'s, CodeUnitWidth8>;

/// `Captures` represents a group of captured byte strings for a single match.
///
/// The 0th capture always corresponds to the entire match. Each subsequent
/// index corresponds to the next capture group in the regex. If a capture
/// group is named, then the matched byte string is *also* available via the
/// `name` method. (Note that the 0th capture is always unnamed and so must be
/// accessed with the `get` method.)
///
/// Positions returned from a capture group are always byte indices.
///
/// `'s` is the lifetime of the matched subject string.
pub type Captures<'s> = CapturesImpl<'s, CodeUnitWidth8>;

#[cfg(test)]
mod tests {
    use super::{CodeUnitWidth8, Regex, RegexBuilder};
    use crate::is_jit_available;

    fn b(string: &str) -> &[u8] {
        string.as_bytes()
    }

    fn find_iter_tuples(re: &Regex, subject: &[u8]) -> Vec<(usize, usize)> {
        let mut tuples = vec![];
        for result in re.find_iter(subject) {
            let m = result.unwrap();
            tuples.push((m.start(), m.end()));
        }
        tuples
    }

    fn cap_iter_tuples(re: &Regex, subject: &[u8]) -> Vec<(usize, usize)> {
        let mut tuples = vec![];
        for result in re.captures_iter(subject) {
            let caps = result.unwrap();
            let m = caps.get(0).unwrap();
            tuples.push((m.start(), m.end()));
        }
        tuples
    }

    #[test]
    fn caseless() {
        let re = RegexBuilder::new().caseless(true).build("a").unwrap();
        assert!(re.is_match(b("A")).unwrap());

        let re =
            RegexBuilder::new().caseless(true).ucp(true).build("β").unwrap();
        assert!(re.is_match(b("Β")).unwrap());
    }

    #[test]
    fn crlf() {
        let re = RegexBuilder::new().crlf(true).build("a$").unwrap();
        let m = re.find(b("a\r\n")).unwrap().unwrap();
        assert_eq!(m.as_pair(), (0, 1));
    }

    #[test]
    fn dotall() {
        let re = RegexBuilder::new().dotall(false).build(".").unwrap();
        assert!(!re.is_match(b("\n")).unwrap());

        let re = RegexBuilder::new().dotall(true).build(".").unwrap();
        assert!(re.is_match(b("\n")).unwrap());
    }

    #[test]
    fn extended() {
        let re = RegexBuilder::new().extended(true).build("a b c").unwrap();
        assert!(re.is_match(b("abc")).unwrap());
    }

    #[test]
    fn multi_line() {
        let re = RegexBuilder::new().multi_line(false).build("^abc$").unwrap();
        assert!(!re.is_match(b("foo\nabc\nbar")).unwrap());

        let re = RegexBuilder::new().multi_line(true).build("^abc$").unwrap();
        assert!(re.is_match(b("foo\nabc\nbar")).unwrap());
    }

    #[test]
    fn ucp() {
        let re = RegexBuilder::new().ucp(false).build(r"\w").unwrap();
        assert!(!re.is_match(b("β")).unwrap());

        let re = RegexBuilder::new().ucp(true).build(r"\w").unwrap();
        assert!(re.is_match(b("β")).unwrap());
    }

    #[test]
    fn utf() {
        let re = RegexBuilder::new().utf(false).build(".").unwrap();
        assert_eq!(re.find(b("β")).unwrap().unwrap().as_pair(), (0, 1));

        let re = RegexBuilder::new().utf(true).build(".").unwrap();
        assert_eq!(re.find(b("β")).unwrap().unwrap().as_pair(), (0, 2));
    }

    #[test]
    fn fmt_debug_works() {
        let re = RegexBuilder::new().utf(false).build(".").unwrap();
        let m = re.find(b("x")).unwrap().unwrap();
        let _ = format!("{:?}", m);
    }

    #[test]
    fn jit4lyfe() {
        if is_jit_available::<CodeUnitWidth8>() {
            let re = RegexBuilder::new().jit(true).build(r"\w").unwrap();
            assert!(re.is_match(b("a")).unwrap());
        } else {
            // Check that if JIT isn't enabled, then we get an error if we
            // require JIT.
            RegexBuilder::new().jit(true).build(r"\w").unwrap_err();
        }
    }

    // Unlike jit4lyfe, this tests that everything works when requesting the
    // JIT only if it's available. In jit4lyfe, we require the JIT or fail.
    // If the JIT isn't available, then in this test, we simply don't use it.
    #[test]
    fn jit_if_available() {
        let re =
            RegexBuilder::new().jit_if_available(true).build(r"\w").unwrap();
        assert!(re.is_match(b("a")).unwrap());
    }

    // This tests a regression caused a segfault in the pcre2 library
    // https://github.com/BurntSushi/rust-pcre2/issues/10
    #[test]
    fn jit_test_lazy_alloc_subject() {
        let subject: Vec<u8> = vec![];

        let re = RegexBuilder::new()
            .jit_if_available(true)
            .build(r"xxxx|xxxx|xxxx")
            .unwrap();
        assert!(!re.is_match(&subject).unwrap());
    }

    #[test]
    fn utf_with_invalid_data() {
        let re = RegexBuilder::new().build(r".").unwrap();
        assert_eq!(re.find(b"\xFF").unwrap().unwrap().as_pair(), (0, 1));

        let re = RegexBuilder::new().utf(true).build(r".").unwrap();
        assert!(re.find(b"\xFF").is_err());
    }

    #[test]
    fn capture_names() {
        let re = RegexBuilder::new()
            .build(r"(?P<foo>abc)|(def)|(?P<a>ghi)|(?P<springsteen>jkl)")
            .unwrap();
        assert_eq!(
            re.capture_names().to_vec(),
            vec![
                None,
                Some("foo".to_string()),
                None,
                Some("a".to_string()),
                Some("springsteen".to_string()),
            ]
        );

        // Test our internal map as well.
        let capture_names_idx = re.get_capture_names_idxs();
        assert_eq!(capture_names_idx.len(), 3);
        assert_eq!(capture_names_idx["foo"], 1);
        assert_eq!(capture_names_idx["a"], 3);
        assert_eq!(capture_names_idx["springsteen"], 4);
    }

    #[test]
    fn captures_get() {
        let re = Regex::new(r"[a-z]+(?:([0-9]+)|([A-Z]+))").unwrap();
        let caps = re.captures(b"abc123").unwrap().unwrap();

        let text1 = caps.get(1).map_or(&b""[..], |m| m.as_bytes());
        let text2 = caps.get(2).map_or(&b""[..], |m| m.as_bytes());
        assert_eq!(text1, &b"123"[..]);
        assert_eq!(text2, &b""[..]);
    }

    #[test]
    fn find_iter_empty() {
        let re = Regex::new(r"(?m:^)").unwrap();
        assert_eq!(find_iter_tuples(&re, b""), vec![(0, 0)]);
        assert_eq!(find_iter_tuples(&re, b"\n"), vec![(0, 0)]);
        assert_eq!(find_iter_tuples(&re, b"\n\n"), vec![(0, 0), (1, 1)]);
        assert_eq!(find_iter_tuples(&re, b"\na\n"), vec![(0, 0), (1, 1)]);
        assert_eq!(
            find_iter_tuples(&re, b"\na\n\n"),
            vec![(0, 0), (1, 1), (3, 3),]
        );
    }

    #[test]
    fn captures_iter_empty() {
        let re = Regex::new(r"(?m:^)").unwrap();
        assert_eq!(cap_iter_tuples(&re, b""), vec![(0, 0)]);
        assert_eq!(cap_iter_tuples(&re, b"\n"), vec![(0, 0)]);
        assert_eq!(cap_iter_tuples(&re, b"\n\n"), vec![(0, 0), (1, 1)]);
        assert_eq!(cap_iter_tuples(&re, b"\na\n"), vec![(0, 0), (1, 1)]);
        assert_eq!(
            cap_iter_tuples(&re, b"\na\n\n"),
            vec![(0, 0), (1, 1), (3, 3),]
        );
    }

    #[test]
    fn max_jit_stack_size_does_something() {
        if !is_jit_available::<CodeUnitWidth8>() {
            return;
        }

        let hundred = "\
            ABCDEFGHIJKLMNOPQRSTUVWXY\
            ABCDEFGHIJKLMNOPQRSTUVWXY\
            ABCDEFGHIJKLMNOPQRSTUVWXY\
            ABCDEFGHIJKLMNOPQRSTUVWXY\
        ";
        let hay = format!("{}", hundred.repeat(100));

        // First, try a regex that checks that we can blow the JIT stack limit.
        let re = RegexBuilder::new()
            .ucp(true)
            .jit(true)
            .max_jit_stack_size(Some(1))
            .build(r"((((\w{10})){100}))+")
            .unwrap();
        let result = re.is_match(hay.as_bytes());
        if result.is_ok() {
            // Skip this test, since for some reason we weren't able to blow
            // the stack limit.
            return;
        }
        let err = result.unwrap_err();
        assert!(err.to_string().contains("JIT stack limit reached"));

        // Now bump up the JIT stack limit and check that it succeeds.
        let re = RegexBuilder::new()
            .ucp(true)
            .jit(true)
            .max_jit_stack_size(Some(1 << 20))
            .build(r"((((\w{10})){100}))+")
            .unwrap();
        assert!(re.is_match(hay.as_bytes()).unwrap());
    }

    #[test]
    fn find_start_end_and_as_bytes() {
        let hay =
            "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        let pattern = r"
            (?x)    (?#: Allow comments and whitespace.)

            [a-z]   (?#: Lowercase letter.)
            +       (?#: One or more times.)
            ";
        let re = RegexBuilder::new()
            .extended(true)
            .utf(true)
            .jit_if_available(true)
            .build(pattern)
            .unwrap();
        let matched = re.find(hay.as_bytes()).unwrap().unwrap();
        assert_eq!(matched.start(), 10);
        assert_eq!(matched.end(), 10 + 26);
        assert_eq!(matched.as_bytes(), b"abcdefghijklmnopqrstuvwxyz");
    }

    #[test]
    fn find_utf_emoji_as_bytes() {
        let hay = "0123456789😀👍🏼🎉abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        let pattern = r"(*UTF)
            (?x)    (?#: Allow comments and whitespace.)

            [^\N{U+0000}-\N{U+007F}]    (?#: Non-ascii code points.)
            +                           (?#: One or more times.)
            ";
        let re = RegexBuilder::new()
            .extended(true)
            .utf(true)
            .jit_if_available(true)
            .build(pattern)
            .unwrap();
        let matched = re.find(hay.as_bytes()).unwrap().unwrap();
        assert_eq!(matched.as_bytes(), "😀👍🏼🎉".as_bytes());
    }
}
