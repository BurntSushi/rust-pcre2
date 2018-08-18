use std::cell::RefCell;
use std::fmt;
use std::ptr;
use std::slice;
use std::sync::Arc;

use pcre2_sys::*;
use thread_local::CachedThreadLocal;

use error::Error;

/// Match represents a single match of a regex in a subject string.
///
/// The lifetime parameter `'s` refers to the lifetime of the matched portion
/// of the subject string.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Match<'s> {
    subject: &'s [u8],
    start: usize,
    end: usize,
}

impl<'s> Match<'s> {
    /// Returns the starting byte offset of the match in the subject.
    #[inline]
    pub fn start(&self) -> usize {
        self.start
    }

    /// Returns the ending byte offset of the match in the subject.
    #[inline]
    pub fn end(&self) -> usize {
        self.end
    }

    /// Returns the matched portion of the subject string.
    #[inline]
    pub fn as_bytes(&self) -> &'s [u8] {
        self.subject
    }

    /// Creates a new match from the given subject string and byte offsets.
    fn new(subject: &'s [u8], start: usize, end: usize) -> Match<'s> {
        Match { subject, start, end }
    }

    #[cfg(test)]
    fn as_pair(&self) -> (usize, usize) {
        (self.start, self.end)
    }
}

#[derive(Clone, Debug)]
struct Config {
    /// PCRE2_CASELESS
    caseless: bool,
    /// PCRE2_DOTALL
    dotall: bool,
    /// PCRE2_EXTENDED
    extended: bool,
    /// PCRE2_MULTILINE
    multiline: bool,
    /// PCRE2_UCP
    ucp: bool,
    /// PCRE2_UTF
    utf: bool,
    /// PCRE2_NO_UTF_CHECK
    utf_check: bool,
    /// use pcre2_jit_compile
    jit: bool,
}

impl Default for Config {
    fn default() -> Config {
        Config {
            caseless: false,
            dotall: false,
            extended: false,
            multiline: false,
            ucp: false,
            utf: false,
            utf_check: true,
            jit: false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct RegexBuilder {
    config: Config,
}

impl RegexBuilder {
    /// Create a new builder with a default configuration.
    pub fn new() -> RegexBuilder {
        RegexBuilder { config: Config::default() }
    }

    /// Compile the given pattern into a PCRE regex using the current
    /// configuration.
    ///
    /// If there was a problem compiling the pattern, then an error is
    /// returned.
    pub fn build(&self, pattern: &str) -> Result<Regex, Error> {
        let mut options = 0;
        if self.config.caseless {
            options |= PCRE2_CASELESS;
        }
        if self.config.dotall {
            options |= PCRE2_DOTALL;
        }
        if self.config.extended {
            options |= PCRE2_EXTENDED;
        }
        if self.config.multiline {
            options |= PCRE2_MULTILINE;
        }
        if self.config.ucp {
            options |= PCRE2_UCP;
            options |= PCRE2_UTF;
        }
        if self.config.utf {
            options |= PCRE2_UTF;
        }

        let mut code = Code::new(pattern, options)?;
        if self.config.jit {
            code.jit_compile()?;
        }
        Ok(Regex {
            config: self.config.clone(),
            pattern: pattern.to_string(),
            code: Arc::new(code),
            match_data: CachedThreadLocal::new(),
        })
    }

    /// Enables case insensitive matching.
    ///
    /// If the `utf` option is also set, then Unicode case folding is used
    /// to determine case insensitivity. When the `utf` option is not set,
    /// then only standard ASCII case insensitivity is considered.
    ///
    /// This option corresponds to the `i` flag.
    pub fn caseless(&mut self, yes: bool) -> &mut RegexBuilder {
        self.config.caseless = yes;
        self
    }

    /// Enables "dot all" matching.
    ///
    /// When enabled, the `.` metacharacter in the pattern matches any
    /// character, include `\n`. When disabled (the default), `.` will match
    /// any character except for `\n`.
    ///
    /// This option corresponds to the `s` flag.
    pub fn dotall(&mut self, yes: bool) -> &mut RegexBuilder {
        self.config.dotall = yes;
        self
    }

    /// Enable "extended" mode in the pattern, where whitespace is ignored.
    ///
    /// This option corresponds to the `x` flag.
    pub fn extended(&mut self, yes: bool) -> &mut RegexBuilder {
        self.config.extended = yes;
        self
    }

    /// Enable multiline matching mode.
    ///
    /// When enabled, the `^` and `$` anchors will match both at the beginning
    /// and end of a subject string, in addition to matching at the start of
    /// a line and the end of a line. When disabled, the `^` and `$` anchors
    /// will only match at the beginning and end of a subject string.
    ///
    /// This option corresponds to the `m` flag.
    pub fn multiline(&mut self, yes: bool) -> &mut RegexBuilder {
        self.config.multiline = yes;
        self
    }

    /// Enable Unicode matching mode.
    ///
    /// When enabled, the following patterns become Unicode aware: `\b`, `\B`,
    /// `\d`, `\D`, `\s`, `\S`, `\w`, `\W`.
    ///
    /// When set, this implies UTF matching mode. It is not possible to enable
    /// Unicode matching mode without enabling UTF matching mode.
    ///
    /// This is disabled by default.
    pub fn ucp(&mut self, yes: bool) -> &mut RegexBuilder {
        self.config.ucp = yes;
        self
    }

    /// Enable UTF matching mode.
    ///
    /// When enabled, characters are treated as sequences of code units that
    /// make up a single codepoint instead of as single bytes. For example,
    /// this will cause `.` to match any single UTF-8 encoded codepoint, where
    /// as when this is disabled, `.` will any single byte (except for `\n` in
    /// both cases, unless "dot all" mode is enabled).
    ///
    /// Note that when UTF matching mode is enabled, every search performed
    /// will do a UTF-8 validation check, which can impact performance. The
    /// UTF-8 check can be disabled via the `disable_utf_check` option, but it
    /// is undefined behavior to enable UTF matching mode and search invalid
    /// UTF-8.
    ///
    /// This is disabled by default.
    pub fn utf(&mut self, yes: bool) -> &mut RegexBuilder {
        self.config.utf = yes;
        self
    }

    /// When UTF matching mode is enabled, this will disable the UTF checking
    /// that PCRE2 will normally perform automatically. If UTF matching mode
    /// is not enabled, then this has no effect.
    ///
    /// UTF checking is enabled by default when UTF matching mode is enabled.
    /// If UTF matching mode is enabled and UTF checking is enabled, then PCRE2
    /// will return an error if you attempt to search a subject string that is
    /// not valid UTF-8.
    ///
    /// # Safety
    ///
    /// It is undefined behavior to disable the UTF check in UTF matching mode
    /// and search a subject string that is not valid UTF-8. When the UTF check
    /// is disabled, callers must guarantee that the subject string is valid
    /// UTF-8.
    pub unsafe fn disable_utf_check(&mut self) -> &mut RegexBuilder {
        self.config.utf_check = false;
        self
    }

    /// Enable PCRE2's JIT.
    ///
    /// This generally speeds up matching quite a bit. The downside is that it
    /// can increase the time it takes to compile a pattern.
    ///
    /// This is disabled by default.
    pub fn jit(&mut self, yes: bool) -> &mut RegexBuilder {
        self.config.jit = yes;
        self
    }
}

/// A compiled PCRE2 regular expression.
///
/// This regex is safe to use from multiple threads simultaneously. For top
/// performance, it is better to clone a new regex for each thread.
pub struct Regex {
    /// The configuration used to build the regex.
    config: Config,
    /// The original pattern string.
    pattern: String,
    /// The underlying compiled PCRE2 object.
    code: Arc<Code>,
    /// Mutable scratch data used by PCRE2 during matching.
    ///
    /// We use the same strategy as Rust's regex crate here, such that each
    /// thread gets its own match data to support using a Regex object from
    /// multiple threads simultaneously. If some match data doesn't exist for
    /// a thread, then a new one is created on demand.
    match_data: CachedThreadLocal<RefCell<MatchData>>,
}

impl Clone for Regex {
    fn clone(&self) -> Regex {
        Regex {
            config: self.config.clone(),
            pattern: self.pattern.clone(),
            code: Arc::clone(&self.code),
            match_data: CachedThreadLocal::new(),
        }
    }
}

impl fmt::Debug for Regex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Regex({:?})", self.pattern)
    }
}

impl Regex {
    pub fn is_match(&self, subject: &[u8]) -> Result<bool, Error> {
        self.find(subject).map(|m| m.is_some())
    }

    pub fn find<'s>(
        &self,
        subject: &'s [u8],
    ) -> Result<Option<Match<'s>>, Error> {
        self.find_at(subject, 0)
    }

    pub fn is_match_at(
        &self,
        subject: &[u8],
        start: usize,
    ) -> Result<bool, Error> {
        self.find_at(subject, start).map(|m| m.is_some())
    }

    pub fn find_at<'s>(
        &self,
        subject: &'s [u8],
        start: usize,
    ) -> Result<Option<Match<'s>>, Error> {
        assert!(
            start <= subject.len(),
            "start ({}) must be <= subject.len() ({})",
            start,
            subject.len()
        );

        let match_data = self.match_data();
        let mut match_data = match_data.borrow_mut();

        let mut options = 0;
        if !self.config.utf_check {
            options |= PCRE2_NO_UTF_CHECK;
        }
        let rc = unsafe {
            pcre2_match_8(
                self.code.as_ptr(),
                subject.as_ptr(),
                subject.len(),
                start,
                options,
                match_data.as_mut_ptr(),
                ptr::null_mut(),
            )
        };
        if rc == PCRE2_ERROR_NOMATCH {
            Ok(None)
        } else if rc > 0 {
            let ovector = match_data.ovector();
            let (s, e) = (ovector[0], ovector[1]);
            Ok(Some(Match::new(&subject[s..e], s, e)))
        } else {
            // We always create match data with
            // pcre2_match_data_create_from_pattern, so the ovector should
            // always be big enough.
            assert!(rc != 0, "ovector should never be too small");
            Err(Error::matching(rc))
        }
    }

    fn match_data(&self) -> &RefCell<MatchData> {
        let create = || Box::new(RefCell::new(MatchData::new(&self.code)));
        self.match_data.get_or(create)
    }
}

/// A low level representation of a compiled PCRE2 code object.
struct Code(*mut pcre2_code_8);

// SAFETY: Compiled PCRE2 code objects are immutable once built and explicitly
// safe to use from multiple threads simultaneously.
//
// One hitch here is that JIT compiling can write into a PCRE2 code object, but
// we only ever JIT compile immediately after first building the code object
// and before making it available to the caller.
unsafe impl Send for Code {}
unsafe impl Sync for Code {}

impl Drop for Code {
    fn drop(&mut self) {
        unsafe { pcre2_code_free_8(self.0) }
    }
}

impl Code {
    /// Compile the given pattern with the given options. If there was a
    /// problem compiling the pattern, then return an error.
    fn new(pattern: &str, options: u32) -> Result<Code, Error> {
        let (mut error_code, mut error_offset) = (0, 0);
        let code = unsafe {
            pcre2_compile_8(
                pattern.as_ptr(),
                pattern.len(),
                options,
                &mut error_code,
                &mut error_offset,
                ptr::null_mut(),
            )
        };
        if code.is_null() {
            unimplemented!()
        } else {
            Ok(Code(code))
        }
    }

    /// JIT compile this code object.
    ///
    /// If there was a problem performing JIT compilation, then this returns
    /// an error.
    fn jit_compile(&mut self) -> Result<(), Error> {
        let error_code = unsafe {
            pcre2_jit_compile_8(self.0, PCRE2_JIT_COMPLETE)
        };
        if error_code == 0 {
            return Ok(())
        }
        unimplemented!()
    }

    /// Return the underlying raw pointer to the code object.
    fn as_ptr(&self) -> *const pcre2_code_8 {
        self.0
    }
}

/// A low level representation of a match data block.
///
/// Technically, a single match data block can be used with multiple regexes
/// (not simultaneously), but in practice, we just create a single match data
/// block for each regex for each thread it's used in.
struct MatchData {
    match_data: *mut pcre2_match_data_8,
    ovector_ptr: *const usize,
    ovector_count: u32,
}

// SAFETY: Match data blocks can be freely sent from one thread to another,
// but they do not support multiple threads using them simultaneously. So we
// implement Send but not Sync.
unsafe impl Send for MatchData {}

impl Drop for MatchData {
    fn drop(&mut self) {
        unsafe { pcre2_match_data_free_8(self.match_data) }
    }
}

impl MatchData {
    /// Create a new match data block from a compiled PCRE2 code object.
    ///
    /// This panics if memory could not be allocated for the block.
    fn new(code: &Code) -> MatchData {
        let match_data = unsafe {
            pcre2_match_data_create_from_pattern_8(
                code.as_ptr(),
                ptr::null_mut(),
            )
        };
        assert!(!match_data.is_null(), "failed to allocate match data block");

        let ovector_ptr = unsafe { pcre2_get_ovector_pointer_8(match_data) };
        assert!(!ovector_ptr.is_null(), "got NULL ovector pointer");
        let ovector_count = unsafe { pcre2_get_ovector_count_8(match_data) };
        MatchData { match_data, ovector_ptr, ovector_count }
    }

    /// Return a mutable reference to the underlying match data.
    fn as_mut_ptr(&mut self) -> *mut pcre2_match_data_8 {
        self.match_data
    }

    /// Return the ovector corresponding to this match data.
    ///
    /// The ovector represents match offsets as pairs. This always returns
    /// N + 1 pairs (so 2*N + 1 offsets), where N is the number of capturing
    /// groups in the original regex.
    fn ovector(&self) -> &[usize] {
        unsafe {
            slice::from_raw_parts(
                self.ovector_ptr,
                self.ovector_count as usize * 2,
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::RegexBuilder;

    fn b(string: &str) -> &[u8] {
        string.as_bytes()
    }

    #[test]
    fn caseless() {
        let re = RegexBuilder::new()
            .caseless(true)
            .build("a")
            .unwrap();
        assert!(re.is_match(b("A")).unwrap());

        let re = RegexBuilder::new()
            .caseless(true)
            .ucp(true)
            .build("β")
            .unwrap();
        assert!(re.is_match(b("Β")).unwrap());
    }

    #[test]
    fn dotall() {
        let re = RegexBuilder::new()
            .dotall(false)
            .build(".")
            .unwrap();
        assert!(!re.is_match(b("\n")).unwrap());

        let re = RegexBuilder::new()
            .dotall(true)
            .build(".")
            .unwrap();
        assert!(re.is_match(b("\n")).unwrap());
    }

    #[test]
    fn extended() {
        let re = RegexBuilder::new()
            .extended(true)
            .build("a b c")
            .unwrap();
        assert!(re.is_match(b("abc")).unwrap());
    }

    #[test]
    fn multiline() {
        let re = RegexBuilder::new()
            .multiline(false)
            .build("^abc$")
            .unwrap();
        assert!(!re.is_match(b("foo\nabc\nbar")).unwrap());

        let re = RegexBuilder::new()
            .multiline(true)
            .build("^abc$")
            .unwrap();
        assert!(re.is_match(b("foo\nabc\nbar")).unwrap());
    }

    #[test]
    fn ucp() {
        let re = RegexBuilder::new()
            .ucp(false)
            .build(r"\w")
            .unwrap();
        assert!(!re.is_match(b("β")).unwrap());

        let re = RegexBuilder::new()
            .ucp(true)
            .build(r"\w")
            .unwrap();
        assert!(re.is_match(b("β")).unwrap());
    }

    #[test]
    fn utf() {
        let re = RegexBuilder::new()
            .utf(false)
            .build(".")
            .unwrap();
        assert_eq!(re.find(b("β")).unwrap().unwrap().as_pair(), (0, 1));

        let re = RegexBuilder::new()
            .utf(true)
            .build(".")
            .unwrap();
        assert_eq!(re.find(b("β")).unwrap().unwrap().as_pair(), (0, 2));
    }

    #[test]
    fn jit4lyfe() {
        let re = RegexBuilder::new()
            .jit(true)
            .build(r"\w")
            .unwrap();
        assert!(re.is_match(b("a")).unwrap());
    }

    #[test]
    fn utf_with_invalid_data() {
        let re = RegexBuilder::new()
            .build(r".")
            .unwrap();
        assert_eq!(re.find(b"\xFF").unwrap().unwrap().as_pair(), (0, 1));

        let re = RegexBuilder::new()
            .utf(true)
            .build(r".")
            .unwrap();
        assert!(re.find(b"\xFF").is_err());
    }
}
