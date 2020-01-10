use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::ops::Index;
use std::sync::Arc;

use log::debug;
use pcre2_sys::{
    PCRE2_CASELESS, PCRE2_DOTALL, PCRE2_EXTENDED, PCRE2_MULTILINE,
    PCRE2_UCP, PCRE2_UTF, PCRE2_NO_UTF_CHECK, PCRE2_UNSET,
    PCRE2_NEWLINE_ANYCRLF,
};
use thread_local::CachedThreadLocal;

use crate::error::Error;
use crate::ffi::{Code, CompileContext, MatchConfig, MatchData};

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
        &self.subject[self.start..self.end]
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
    multi_line: bool,
    /// PCRE2_NEWLINE_ANYCRLF
    crlf: bool,
    /// PCRE2_UCP
    ucp: bool,
    /// PCRE2_UTF
    utf: bool,
    /// PCRE2_NO_UTF_CHECK
    utf_check: bool,
    /// use pcre2_jit_compile
    jit: JITChoice,
    /// Match-time specific configuration knobs.
    match_config: MatchConfig,
}

#[derive(Clone, Debug)]
enum JITChoice {
    /// Never do JIT compilation.
    Never,
    /// Always do JIT compilation and return an error if it fails.
    Always,
    /// Attempt to do JIT compilation but silently fall back to non-JIT.
    Attempt,
}

impl Default for Config {
    fn default() -> Config {
        Config {
            caseless: false,
            dotall: false,
            extended: false,
            multi_line: false,
            crlf: false,
            ucp: false,
            utf: false,
            utf_check: true,
            jit: JITChoice::Never,
            match_config: MatchConfig::default(),
        }
    }
}

/// A builder for configuring the compilation of a PCRE2 regex.
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
        if self.config.multi_line {
            options |= PCRE2_MULTILINE;
        }
        if self.config.ucp {
            options |= PCRE2_UCP;
            options |= PCRE2_UTF;
        }
        if self.config.utf {
            options |= PCRE2_UTF;
        }

        let mut ctx = CompileContext::new();
        if self.config.crlf {
            ctx.set_newline(PCRE2_NEWLINE_ANYCRLF)
                .expect("PCRE2_NEWLINE_ANYCRLF is a legal value");
        }

        let mut code = Code::new(pattern, options, ctx)?;
        match self.config.jit {
            JITChoice::Never => {} // fallthrough
            JITChoice::Always => {
                code.jit_compile()?;
            }
            JITChoice::Attempt => {
                if let Err(err) = code.jit_compile() {
                    debug!("JIT compilation failed: {}", err);
                }
            }
        }
        let capture_names = code.capture_names()?;
        let mut idx = HashMap::new();
        for (i, group) in capture_names.iter().enumerate() {
            if let Some(ref name) = *group {
                idx.insert(name.to_string(), i);
            }
        }
        Ok(Regex {
            config: Arc::new(self.config.clone()),
            pattern: pattern.to_string(),
            code: Arc::new(code),
            capture_names: Arc::new(capture_names),
            capture_names_idx: Arc::new(idx),
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
    pub fn multi_line(&mut self, yes: bool) -> &mut RegexBuilder {
        self.config.multi_line = yes;
        self
    }

    /// Enable matching of CRLF as a line terminator.
    ///
    /// When enabled, anchors such as `^` and `$` will match any of the
    /// following as a line terminator: `\r`, `\n` or `\r\n`.
    ///
    /// This is disabled by default, in which case, only `\n` is recognized as
    /// a line terminator.
    pub fn crlf(&mut self, yes: bool) -> &mut RegexBuilder {
        self.config.crlf = yes;
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

    /// Enable PCRE2's JIT and return an error if it's not available.
    ///
    /// This generally speeds up matching quite a bit. The downside is that it
    /// can increase the time it takes to compile a pattern.
    ///
    /// If the JIT isn't available or if JIT compilation returns an error, then
    /// regex compilation will fail with the corresponding error.
    ///
    /// This is disabled by default, and always overrides `jit_if_available`.
    pub fn jit(&mut self, yes: bool) -> &mut RegexBuilder {
        if yes {
            self.config.jit = JITChoice::Always;
        } else {
            self.config.jit = JITChoice::Never;
        }
        self
    }

    /// Enable PCRE2's JIT if it's available.
    ///
    /// This generally speeds up matching quite a bit. The downside is that it
    /// can increase the time it takes to compile a pattern.
    ///
    /// If the JIT isn't available or if JIT compilation returns an error,
    /// then a debug message with the error will be emitted and the regex will
    /// otherwise silently fall back to non-JIT matching.
    ///
    /// This is disabled by default, and always overrides `jit`.
    pub fn jit_if_available(&mut self, yes: bool) -> &mut RegexBuilder {
        if yes {
            self.config.jit = JITChoice::Attempt;
        } else {
            self.config.jit = JITChoice::Never;
        }
        self
    }

    /// Set the maximum size of PCRE2's JIT stack, in bytes. If the JIT is
    /// not enabled, then this has no effect.
    ///
    /// When `None` is given, no custom JIT stack will be created, and instead,
    /// the default JIT stack is used. When the default is used, its maximum
    /// size is 32 KB.
    ///
    /// When this is set, then a new JIT stack will be created with the given
    /// maximum size as its limit.
    ///
    /// Increasing the stack size can be useful for larger regular expressions.
    ///
    /// By default, this is set to `None`.
    pub fn max_jit_stack_size(
        &mut self,
        bytes: Option<usize>,
    ) -> &mut RegexBuilder {
        self.config.match_config.max_jit_stack_size = bytes;
        self
    }
}

/// A compiled PCRE2 regular expression.
///
/// This regex is safe to use from multiple threads simultaneously. For top
/// performance, it is better to clone a new regex for each thread.
pub struct Regex {
    /// The configuration used to build the regex.
    config: Arc<Config>,
    /// The original pattern string.
    pattern: String,
    /// The underlying compiled PCRE2 object.
    code: Arc<Code>,
    /// The capture group names for this regex.
    capture_names: Arc<Vec<Option<String>>>,
    /// A map from capture group name to capture group index.
    capture_names_idx: Arc<HashMap<String, usize>>,
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
            config: Arc::clone(&self.config),
            pattern: self.pattern.clone(),
            code: Arc::clone(&self.code),
            capture_names: Arc::clone(&self.capture_names),
            capture_names_idx: Arc::clone(&self.capture_names_idx),
            match_data: CachedThreadLocal::new(),
        }
    }
}

impl fmt::Debug for Regex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Regex({:?})", self.pattern)
    }
}

impl Regex {
    /// Compiles a regular expression using the default configuration.
    ///
    /// Once compiled, it can be used repeatedly to search, split or replace
    /// text in a string.
    ///
    /// If an invalid expression is given, then an error is returned.
    ///
    /// To configure compilation options for the regex, use the
    /// [`RegexBuilder`](struct.RegexBuilder.html).
    pub fn new(pattern: &str) -> Result<Regex, Error> {
        RegexBuilder::new().build(pattern)
    }

    /// Returns true if and only if the regex matches the subject string given.
    ///
    /// # Example
    ///
    /// Test if some text contains at least one word with exactly 13 ASCII word
    /// bytes:
    ///
    /// ```rust
    /// # fn example() -> Result<(), ::pcre2::Error> {
    /// use pcre2::bytes::Regex;
    ///
    /// let text = b"I categorically deny having triskaidekaphobia.";
    /// assert!(Regex::new(r"\b\w{13}\b")?.is_match(text)?);
    /// # Ok(()) }; example().unwrap()
    /// ```
    pub fn is_match(&self, subject: &[u8]) -> Result<bool, Error> {
        self.is_match_at(subject, 0)
    }

    /// Returns the start and end byte range of the leftmost-first match in
    /// `subject`. If no match exists, then `None` is returned.
    ///
    /// # Example
    ///
    /// Find the start and end location of the first word with exactly 13
    /// ASCII word bytes:
    ///
    /// ```rust
    /// # fn example() -> Result<(), ::pcre2::Error> {
    /// use pcre2::bytes::Regex;
    ///
    /// let text = b"I categorically deny having triskaidekaphobia.";
    /// let mat = Regex::new(r"\b\w{13}\b")?.find(text)?.unwrap();
    /// assert_eq!((mat.start(), mat.end()), (2, 15));
    /// # Ok(()) }; example().unwrap()
    /// ```
    pub fn find<'s>(
        &self,
        subject: &'s [u8],
    ) -> Result<Option<Match<'s>>, Error> {
        self.find_at(subject, 0)
    }

    /// Returns an iterator for each successive non-overlapping match in
    /// `subject`, returning the start and end byte indices with respect to
    /// `subject`.
    ///
    /// # Example
    ///
    /// Find the start and end location of every word with exactly 13 ASCII
    /// word bytes:
    ///
    /// ```rust
    /// # fn example() -> Result<(), ::pcre2::Error> {
    /// use pcre2::bytes::Regex;
    ///
    /// let text = b"Retroactively relinquishing remunerations is reprehensible.";
    /// for result in Regex::new(r"\b\w{13}\b")?.find_iter(text) {
    ///     let mat = result?;
    ///     println!("{:?}", mat);
    /// }
    /// # Ok(()) }; example().unwrap()
    /// ```
    pub fn find_iter<'r, 's>(&'r self, subject: &'s [u8]) -> Matches<'r, 's> {
        Matches {
            re: self,
            match_data: self.match_data(),
            subject: subject,
            last_end: 0,
            last_match: None,
        }
    }

    /// Returns the capture groups corresponding to the leftmost-first
    /// match in `subject`. Capture group `0` always corresponds to the entire
    /// match. If no match is found, then `None` is returned.
    ///
    /// # Examples
    ///
    /// Say you have some text with movie names and their release years,
    /// like "'Citizen Kane' (1941)". It'd be nice if we could search for text
    /// looking like that, while also extracting the movie name and its release
    /// year separately.
    ///
    /// ```rust
    /// # fn example() -> Result<(), ::pcre2::Error> {
    /// use pcre2::bytes::Regex;
    ///
    /// let re = Regex::new(r"'([^']+)'\s+\((\d{4})\)")?;
    /// let text = b"Not my favorite movie: 'Citizen Kane' (1941).";
    /// let caps = re.captures(text)?.unwrap();
    /// assert_eq!(&caps[1], &b"Citizen Kane"[..]);
    /// assert_eq!(&caps[2], &b"1941"[..]);
    /// assert_eq!(&caps[0], &b"'Citizen Kane' (1941)"[..]);
    /// // You can also access the groups by index using the Index notation.
    /// // Note that this will panic on an invalid index.
    /// assert_eq!(&caps[1], b"Citizen Kane");
    /// assert_eq!(&caps[2], b"1941");
    /// assert_eq!(&caps[0], b"'Citizen Kane' (1941)");
    /// # Ok(()) }; example().unwrap()
    /// ```
    ///
    /// Note that the full match is at capture group `0`. Each subsequent
    /// capture group is indexed by the order of its opening `(`.
    ///
    /// We can make this example a bit clearer by using *named* capture groups:
    ///
    /// ```rust
    /// # fn example() -> Result<(), ::pcre2::Error> {
    /// use pcre2::bytes::Regex;
    ///
    /// let re = Regex::new(r"'(?P<title>[^']+)'\s+\((?P<year>\d{4})\)")?;
    /// let text = b"Not my favorite movie: 'Citizen Kane' (1941).";
    /// let caps = re.captures(text)?.unwrap();
    /// assert_eq!(&caps["title"], &b"Citizen Kane"[..]);
    /// assert_eq!(&caps["year"], &b"1941"[..]);
    /// assert_eq!(&caps[0], &b"'Citizen Kane' (1941)"[..]);
    /// // You can also access the groups by name using the Index notation.
    /// // Note that this will panic on an invalid group name.
    /// assert_eq!(&caps["title"], b"Citizen Kane");
    /// assert_eq!(&caps["year"], b"1941");
    /// assert_eq!(&caps[0], b"'Citizen Kane' (1941)");
    /// # Ok(()) }; example().unwrap()
    /// ```
    ///
    /// Here we name the capture groups, which we can access with the `name`
    /// method or the `Index` notation with a `&str`. Note that the named
    /// capture groups are still accessible with `get` or the `Index` notation
    /// with a `usize`.
    ///
    /// The `0`th capture group is always unnamed, so it must always be
    /// accessed with `get(0)` or `[0]`.
    pub fn captures<'s>(
        &self,
        subject: &'s [u8],
    ) -> Result<Option<Captures<'s>>, Error> {
        let mut locs = self.capture_locations();
        Ok(self.captures_read(&mut locs, subject)?.map(move |_| Captures {
            subject: subject,
            locs: locs,
            idx: Arc::clone(&self.capture_names_idx),
        }))
    }

    /// Returns an iterator over all the non-overlapping capture groups matched
    /// in `subject`. This is operationally the same as `find_iter`, except it
    /// yields information about capturing group matches.
    ///
    /// # Example
    ///
    /// We can use this to find all movie titles and their release years in
    /// some text, where the movie is formatted like "'Title' (xxxx)":
    ///
    /// ```rust
    /// # fn example() -> Result<(), ::pcre2::Error> {
    /// use std::str;
    ///
    /// use pcre2::bytes::Regex;
    ///
    /// let re = Regex::new(r"'(?P<title>[^']+)'\s+\((?P<year>\d{4})\)")?;
    /// let text = b"'Citizen Kane' (1941), 'The Wizard of Oz' (1939), 'M' (1931).";
    /// for result in re.captures_iter(text) {
    ///     let caps = result?;
    ///     let title = str::from_utf8(&caps["title"]).unwrap();
    ///     let year = str::from_utf8(&caps["year"]).unwrap();
    ///     println!("Movie: {:?}, Released: {:?}", title, year);
    /// }
    /// // Output:
    /// // Movie: Citizen Kane, Released: 1941
    /// // Movie: The Wizard of Oz, Released: 1939
    /// // Movie: M, Released: 1931
    /// # Ok(()) }; example().unwrap()
    /// ```
    pub fn captures_iter<'r, 's>(
        &'r self,
        subject: &'s [u8],
    ) -> CaptureMatches<'r, 's> {
        CaptureMatches {
            re: self,
            subject: subject,
            last_end: 0,
            last_match: None,
        }
    }
}

/// Advanced or  "lower level" search methods.
impl Regex {
    /// Returns the same as is_match, but starts the search at the given
    /// offset.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    pub fn is_match_at(
        &self,
        subject: &[u8],
        start: usize,
    ) -> Result<bool, Error> {
        assert!(
            start <= subject.len(),
            "start ({}) must be <= subject.len() ({})",
            start,
            subject.len()
        );

        let mut options = 0;
        if !self.config.utf_check {
            options |= PCRE2_NO_UTF_CHECK;
        }

        let match_data = self.match_data();
        let mut match_data = match_data.borrow_mut();
        // SAFETY: The only unsafe PCRE2 option we potentially use here is
        // PCRE2_NO_UTF_CHECK, and that only occurs if the caller executes the
        // `disable_utf_check` method, which propagates the safety contract to
        // the caller.
        Ok(unsafe { match_data.find(&self.code, subject, start, options)? })
    }

    /// Returns the same as find, but starts the search at the given
    /// offset.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    pub fn find_at<'s>(
        &self,
        subject: &'s [u8],
        start: usize,
    ) -> Result<Option<Match<'s>>, Error> {
        self.find_at_with_match_data(self.match_data(), subject, start)
    }

    /// Like find_at, but accepts match data instead of acquiring one itself.
    ///
    /// This is useful for implementing the iterator, which permits avoiding
    /// the synchronization overhead of acquiring the match data.
    #[inline(always)]
    fn find_at_with_match_data<'s>(
        &self,
        match_data: &RefCell<MatchData>,
        subject: &'s [u8],
        start: usize,
    ) -> Result<Option<Match<'s>>, Error> {
        assert!(
            start <= subject.len(),
            "start ({}) must be <= subject.len() ({})",
            start,
            subject.len()
        );

        let mut options = 0;
        if !self.config.utf_check {
            options |= PCRE2_NO_UTF_CHECK;
        }

        let mut match_data = match_data.borrow_mut();
        // SAFETY: The only unsafe PCRE2 option we potentially use here is
        // PCRE2_NO_UTF_CHECK, and that only occurs if the caller executes the
        // `disable_utf_check` method, which propagates the safety contract to
        // the caller.
        if unsafe { !match_data.find(&self.code, subject, start, options)? } {
            return Ok(None);
        }
        let ovector = match_data.ovector();
        let (s, e) = (ovector[0], ovector[1]);
        Ok(Some(Match::new(&subject[s..e], s, e)))
    }

    /// This is like `captures`, but uses
    /// [`CaptureLocations`](struct.CaptureLocations.html)
    /// instead of
    /// [`Captures`](struct.Captures.html) in order to amortize allocations.
    ///
    /// To create a `CaptureLocations` value, use the
    /// `Regex::capture_locations` method.
    ///
    /// This returns the overall match if this was successful, which is always
    /// equivalent to the `0`th capture group.
    pub fn captures_read<'s>(
        &self,
        locs: &mut CaptureLocations,
        subject: &'s [u8],
    ) -> Result<Option<Match<'s>>, Error> {
        self.captures_read_at(locs, subject, 0)
    }

    /// Returns the same as `captures_read`, but starts the search at the given
    /// offset and populates the capture locations given.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    pub fn captures_read_at<'s>(
        &self,
        locs: &mut CaptureLocations,
        subject: &'s [u8],
        start: usize,
    ) -> Result<Option<Match<'s>>, Error> {
        assert!(
            start <= subject.len(),
            "start ({}) must be <= subject.len() ({})",
            start,
            subject.len()
        );

        let mut options = 0;
        if !self.config.utf_check {
            options |= PCRE2_NO_UTF_CHECK;
        }
        // SAFETY: The only unsafe PCRE2 option we potentially use here is
        // PCRE2_NO_UTF_CHECK, and that only occurs if the caller executes the
        // `disable_utf_check` method, which propagates the safety contract to
        // the caller.
        if unsafe { !locs.data.find(&self.code, subject, start, options)? } {
            return Ok(None);
        }
        let ovector = locs.data.ovector();
        let (s, e) = (ovector[0], ovector[1]);
        Ok(Some(Match::new(&subject[s..e], s, e)))
    }
}

/// Auxiliary methods.
impl Regex {
    /// Returns the original pattern string for this regex.
    pub fn as_str(&self) -> &str {
        &self.pattern
    }

    /// Returns a sequence of all capturing groups and their names, if present.
    ///
    /// The length of the slice returned is always equal to the result of
    /// `captures_len`, which is the number of capturing groups (including the
    /// capturing group for the entire pattern).
    ///
    /// Each entry in the slice is the name of the corresponding capturing
    /// group, if one exists. The first capturing group (at index `0`) is
    /// always unnamed.
    ///
    /// Capturing groups are indexed by the order of the opening parenthesis.
    pub fn capture_names(&self) -> &[Option<String>] {
        &self.capture_names
    }

    /// Returns the number of capturing groups in the pattern.
    ///
    /// This is always 1 more than the number of syntactic groups in the
    /// pattern, since the first group always corresponds to the entire match.
    pub fn captures_len(&self) -> usize {
        self.code.capture_count().expect("a valid capture count from PCRE2")
    }

    /// Returns an empty set of capture locations that can be reused in
    /// multiple calls to `captures_read` or `captures_read_at`.
    pub fn capture_locations(&self) -> CaptureLocations {
        CaptureLocations {
            code: Arc::clone(&self.code),
            data: self.new_match_data(),
        }
    }

    fn match_data(&self) -> &RefCell<MatchData> {
        let create = || RefCell::new(self.new_match_data());
        self.match_data.get_or(create)
    }

    fn new_match_data(&self) -> MatchData {
        MatchData::new(self.config.match_config.clone(), &self.code)
    }
}

/// CaptureLocations is a low level representation of the raw offsets of each
/// submatch.
///
/// Primarily, this type is useful when using `Regex` APIs such as
/// `captures_read`, which permits amortizing the allocation in which capture
/// match locations are stored.
///
/// In order to build a value of this type, you'll need to call the
/// `capture_locations` method on the `Regex` being used to execute the search.
/// The value returned can then be reused in subsequent searches.
pub struct CaptureLocations {
    code: Arc<Code>,
    data: MatchData,
}

impl Clone for CaptureLocations {
    fn clone(&self) -> CaptureLocations {
        CaptureLocations {
            code: Arc::clone(&self.code),
            data: MatchData::new(self.data.config().clone(), &self.code),
        }
    }
}

impl fmt::Debug for CaptureLocations {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut offsets: Vec<Option<usize>> = vec![];
        for &offset in self.data.ovector() {
            if offset == PCRE2_UNSET {
                offsets.push(None);
            } else {
                offsets.push(Some(offset));
            }
        }
        write!(f, "CaptureLocations(")?;
        f.debug_list().entries(offsets).finish()?;
        write!(f, ")")
    }
}

impl CaptureLocations {
    /// Returns the start and end positions of the Nth capture group.
    ///
    /// This returns `None` if `i` is not a valid capture group or if the
    /// capture group did not match anything.
    ///
    /// The positions returned are always byte indices with respect to the
    /// original subject string matched.
    #[inline]
    pub fn get(&self, i: usize) -> Option<(usize, usize)> {
        let ovec = self.data.ovector();
        let s = match ovec.get(i * 2) {
            None => return None,
            Some(&s) if s == PCRE2_UNSET => return None,
            Some(&s) => s,
        };
        let e = match ovec.get(i * 2 + 1) {
            None => return None,
            Some(&e) if e == PCRE2_UNSET => return None,
            Some(&e) => e,
        };
        Some((s, e))
    }

    /// Returns the total number of capturing groups.
    ///
    /// This is always at least `1` since every regex has at least `1`
    /// capturing group that corresponds to the entire match.
    #[inline]
    pub fn len(&self) -> usize {
        self.data.ovector().len() / 2
    }
}

/// Captures represents a group of captured byte strings for a single match.
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
pub struct Captures<'s> {
    subject: &'s [u8],
    locs: CaptureLocations,
    idx: Arc<HashMap<String, usize>>,
}

impl<'s> Captures<'s> {
    /// Returns the match associated with the capture group at index `i`. If
    /// `i` does not correspond to a capture group, or if the capture group
    /// did not participate in the match, then `None` is returned.
    ///
    /// # Examples
    ///
    /// Get the text of the match with a default of an empty string if this
    /// group didn't participate in the match:
    ///
    /// ```rust
    /// # fn example() -> Result<(), ::pcre2::Error> {
    /// use pcre2::bytes::Regex;
    ///
    /// let re = Regex::new(r"[a-z]+(?:([0-9]+)|([A-Z]+))")?;
    /// let caps = re.captures(b"abc123")?.unwrap();
    ///
    /// let text1 = caps.get(1).map_or(&b""[..], |m| m.as_bytes());
    /// let text2 = caps.get(2).map_or(&b""[..], |m| m.as_bytes());
    /// assert_eq!(text1, &b"123"[..]);
    /// assert_eq!(text2, &b""[..]);
    /// # Ok(()) }; example().unwrap()
    /// ```
    pub fn get(&self, i: usize) -> Option<Match<'s>> {
        self.locs.get(i).map(|(s, e)| Match::new(self.subject, s, e))
    }

    /// Returns the match for the capture group named `name`. If `name` isn't a
    /// valid capture group or didn't match anything, then `None` is returned.
    pub fn name(&self, name: &str) -> Option<Match<'s>> {
        self.idx.get(name).and_then(|&i| self.get(i))
    }

    /// Returns the number of captured groups.
    ///
    /// This is always at least `1`, since every regex has at least one capture
    /// group that corresponds to the full match.
    #[inline]
    pub fn len(&self) -> usize {
        self.locs.len()
    }
}

impl<'s> fmt::Debug for Captures<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Captures").field(&CapturesDebug(self)).finish()
    }
}

struct CapturesDebug<'c, 's: 'c>(&'c Captures<'s>);

impl<'c, 's> fmt::Debug for CapturesDebug<'c, 's> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn escape_bytes(bytes: &[u8]) -> String {
            let mut s = String::new();
            for &b in bytes {
                s.push_str(&escape_byte(b));
            }
            s
        }

        fn escape_byte(byte: u8) -> String {
            use std::ascii::escape_default;

            let escaped: Vec<u8> = escape_default(byte).collect();
            String::from_utf8_lossy(&escaped).into_owned()
        }

        // We'd like to show something nice here, even if it means an
        // allocation to build a reverse index.
        let slot_to_name: HashMap<&usize, &String> =
            self.0.idx.iter().map(|(a, b)| (b, a)).collect();
        let mut map = f.debug_map();
        for slot in 0..self.0.len() {
            let m = self.0.locs.get(slot).map(|(s, e)| {
                escape_bytes(&self.0.subject[s..e])
            });
            if let Some(name) = slot_to_name.get(&slot) {
                map.entry(&name, &m);
            } else {
                map.entry(&slot, &m);
            }
        }
        map.finish()
    }
}

/// Get a group by index.
///
/// `'s` is the lifetime of the matched subject string.
///
/// The subject can't outlive the `Captures` object if this method is
/// used, because of how `Index` is defined (normally `a[i]` is part
/// of `a` and can't outlive it); to do that, use `get()` instead.
///
/// # Panics
///
/// If there is no group at the given index.
impl<'s> Index<usize> for Captures<'s> {
    type Output = [u8];

    fn index(&self, i: usize) -> &[u8] {
        self.get(i).map(|m| m.as_bytes())
            .unwrap_or_else(|| panic!("no group at index '{}'", i))
    }
}

/// Get a group by name.
///
/// `'s` is the lifetime of the matched subject string and `'i` is the lifetime
/// of the group name (the index).
///
/// The text can't outlive the `Captures` object if this method is
/// used, because of how `Index` is defined (normally `a[i]` is part
/// of `a` and can't outlive it); to do that, use `name` instead.
///
/// # Panics
///
/// If there is no group named by the given value.
impl<'s, 'i> Index<&'i str> for Captures<'s> {
    type Output = [u8];

    fn index<'a>(&'a self, name: &'i str) -> &'a [u8] {
        self.name(name).map(|m| m.as_bytes())
            .unwrap_or_else(|| panic!("no group named '{}'", name))
    }
}

/// An iterator over all non-overlapping matches for a particular subject
/// string.
///
/// The iterator yields matches (if no error occurred while searching)
/// corresponding to the start and end of the match. The indices are byte
/// offsets. The iterator stops when no more matches can be found.
///
/// `'r` is the lifetime of the compiled regular expression and `'s` is the
/// lifetime of the subject string.
pub struct Matches<'r, 's> {
    re: &'r Regex,
    match_data: &'r RefCell<MatchData>,
    subject: &'s [u8],
    last_end: usize,
    last_match: Option<usize>,
}

impl<'r, 's> Iterator for Matches<'r, 's> {
    type Item = Result<Match<'s>, Error>;

    fn next(&mut self) -> Option<Result<Match<'s>, Error>> {
        if self.last_end > self.subject.len() {
            return None;
        }
        let res = self.re.find_at_with_match_data(
            self.match_data,
            self.subject,
            self.last_end,
        );
        let m = match res {
            Err(err) => return Some(Err(err)),
            Ok(None) => return None,
            Ok(Some(m)) => m,
        };
        if m.start() == m.end() {
            // This is an empty match. To ensure we make progress, start
            // the next search at the smallest possible starting position
            // of the next match following this one.
            self.last_end = m.end() + 1;
            // Don't accept empty matches immediately following a match.
            // Just move on to the next match.
            if Some(m.end()) == self.last_match {
                return self.next();
            }
        } else {
            self.last_end = m.end();
        }
        self.last_match = Some(m.end());
        Some(Ok(m))
    }
}

/// An iterator that yields all non-overlapping capture groups matching a
/// particular regular expression.
///
/// The iterator stops when no more matches can be found.
///
/// `'r` is the lifetime of the compiled regular expression and `'s` is the
/// lifetime of the subject string.
pub struct CaptureMatches<'r, 's> {
    re: &'r Regex,
    subject: &'s [u8],
    last_end: usize,
    last_match: Option<usize>,
}

impl<'r, 's> Iterator for CaptureMatches<'r, 's> {
    type Item = Result<Captures<'s>, Error>;

    fn next(&mut self) -> Option<Result<Captures<'s>, Error>> {
        if self.last_end > self.subject.len() {
            return None;
        }
        let mut locs = self.re.capture_locations();
        let res = self.re.captures_read_at(
            &mut locs,
            self.subject,
            self.last_end,
        );
        let m = match res {
            Err(err) => return Some(Err(err)),
            Ok(None) => return None,
            Ok(Some(m)) => m,
        };
        if m.start() == m.end() {
            // This is an empty match. To ensure we make progress, start
            // the next search at the smallest possible starting position
            // of the next match following this one.
            self.last_end = m.end() + 1;
            // Don't accept empty matches immediately following a match.
            // Just move on to the next match.
            if Some(m.end()) == self.last_match {
                return self.next();
            }
        } else {
            self.last_end = m.end();
        }
        self.last_match = Some(m.end());
        Some(Ok(Captures {
            subject: self.subject,
            locs: locs,
            idx: Arc::clone(&self.re.capture_names_idx),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::{Regex, RegexBuilder};
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
    fn crlf() {
        let re = RegexBuilder::new()
            .crlf(true)
            .build("a$")
            .unwrap();
        let m = re.find(b("a\r\n")).unwrap().unwrap();
        assert_eq!(m.as_pair(), (0, 1));
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
    fn multi_line() {
        let re = RegexBuilder::new()
            .multi_line(false)
            .build("^abc$")
            .unwrap();
        assert!(!re.is_match(b("foo\nabc\nbar")).unwrap());

        let re = RegexBuilder::new()
            .multi_line(true)
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
        if is_jit_available() {
            let re = RegexBuilder::new()
                .jit(true)
                .build(r"\w")
                .unwrap();
            assert!(re.is_match(b("a")).unwrap());
        } else {
            // Check that if JIT isn't enabled, then we get an error if we
            // require JIT.
            RegexBuilder::new()
                .jit(true)
                .build(r"\w")
                .unwrap_err();
        }
    }

    // Unlike jit4lyfe, this tests that everything works when requesting the
    // JIT only if it's available. In jit4lyfe, we require the JIT or fail.
    // If the JIT isn't available, then in this test, we simply don't use it.
    #[test]
    fn jit_if_available() {
        let re = RegexBuilder::new()
            .jit_if_available(true)
            .build(r"\w")
            .unwrap();
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

    #[test]
    fn capture_names() {
        let re = RegexBuilder::new()
            .build(
                r"(?P<foo>abc)|(def)|(?P<a>ghi)|(?P<springsteen>jkl)"
            )
            .unwrap();
        assert_eq!(re.capture_names().to_vec(), vec![
            None,
            Some("foo".to_string()),
            None,
            Some("a".to_string()),
            Some("springsteen".to_string()),
        ]);

        // Test our internal map as well.
        assert_eq!(re.capture_names_idx.len(), 3);
        assert_eq!(re.capture_names_idx["foo"], 1);
        assert_eq!(re.capture_names_idx["a"], 3);
        assert_eq!(re.capture_names_idx["springsteen"], 4);
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
        assert_eq!(find_iter_tuples(&re, b"\na\n\n"), vec![
            (0, 0), (1, 1), (3, 3),
        ]);
    }

    #[test]
    fn captures_iter_empty() {
        let re = Regex::new(r"(?m:^)").unwrap();
        assert_eq!(cap_iter_tuples(&re, b""), vec![(0, 0)]);
        assert_eq!(cap_iter_tuples(&re, b"\n"), vec![(0, 0)]);
        assert_eq!(cap_iter_tuples(&re, b"\n\n"), vec![(0, 0), (1, 1)]);
        assert_eq!(cap_iter_tuples(&re, b"\na\n"), vec![(0, 0), (1, 1)]);
        assert_eq!(cap_iter_tuples(&re, b"\na\n\n"), vec![
            (0, 0), (1, 1), (3, 3),
        ]);
    }

    #[test]
    fn max_jit_stack_size_does_something() {
        if !is_jit_available() {
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
            .max_jit_stack_size(Some(1<<20))
            .build(r"((((\w{10})){100}))+")
            .unwrap();
        assert!(re.is_match(hay.as_bytes()).unwrap());
    }
}
