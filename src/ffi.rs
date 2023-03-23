/*!
This module defines a low level and *mostly* safe abstraction around the
core PCRE2 regex primitives. Callers may still need to deal with some
unsafety, but this layer will take care of the obvious things, such as
resource management and error handling.
*/

use crate::error::Error;
use std::{
    cmp,
    marker::PhantomData,
    panic::{RefUnwindSafe, UnwindSafe},
    ptr, slice,
};
use {libc::c_void, pcre2_sys::*};

pub trait NameTableEntry {
    /// The index of the named subpattern.
    fn index(&self) -> usize;

    /// The name of the named subpattern.
    fn name(&self) -> String;
}

#[allow(non_camel_case_types)]
#[repr(C)]
pub struct name_table_entry_8 {
    match_index_msb: u8,
    match_index_lsb: u8,

    // In C, the 'name' field is a flexible array member.
    // This does not contribute to the sizeof the struct.
    name: u8,
}

#[allow(non_camel_case_types)]
#[repr(C)]
pub struct name_table_entry_32 {
    match_index: u32,
    name: u32, // See above re: flexible array member
}

impl NameTableEntry for name_table_entry_8 {
    fn index(&self) -> usize {
        ((self.match_index_msb as usize) << 8)
            | (self.match_index_lsb as usize)
    }

    fn name(&self) -> String {
        // The name is nul-terminated.
        let name = &self.name as *const u8;
        let mut len = 0;
        while unsafe { *name.offset(len as isize) } != 0 {
            len += 1;
        }
        let bytes = unsafe { slice::from_raw_parts(name, len) };
        String::from_utf8_lossy(bytes).into_owned()
    }
}

impl NameTableEntry for name_table_entry_32 {
    fn index(&self) -> usize {
        self.match_index as usize
    }

    fn name(&self) -> String {
        // The name is nul-terminated.
        let replacement: char = '\u{FFFD}';
        let name = &self.name as *const u32;
        let mut len = 0;
        let mut result = String::new();
        loop {
            let c = unsafe { *name.offset(len) };
            if c == 0 {
                break;
            }
            result.push(char::from_u32(c).unwrap_or(replacement));
            len += 1;
        }
        result
    }
}

#[allow(non_camel_case_types)]
pub trait CodeUnitWidth: std::fmt::Debug + 'static {
    type pcre2_code: UnwindSafe + RefUnwindSafe;
    type pcre2_compile_context: UnwindSafe + RefUnwindSafe;
    type pcre2_match_context;
    type pcre2_match_data;
    type pcre2_jit_stack;
    type PCRE2_CHAR;
    type PCRE2_SPTR;
    type name_table_entry: NameTableEntry;
    type SubjectChar: Copy;
    type Pattern: Clone + std::fmt::Debug;

    fn escape_subject(subject: &[Self::SubjectChar]) -> String;

    fn pattern_to_sptr_len(
        pattern: &Self::Pattern,
    ) -> (Self::PCRE2_SPTR, usize);
    fn subject_to_sptr_len(
        subject: &[Self::SubjectChar],
    ) -> (Self::PCRE2_SPTR, usize);

    unsafe fn pcre2_config(
        arg1: u32,
        arg2: *mut ::libc::c_void,
    ) -> ::libc::c_int;
    unsafe fn pcre2_code_free(arg1: *mut Self::pcre2_code);
    unsafe fn pcre2_compile(
        arg1: Self::PCRE2_SPTR,
        arg2: usize,
        arg3: u32,
        arg4: *mut ::libc::c_int,
        arg5: *mut ::libc::size_t,
        arg6: *mut Self::pcre2_compile_context,
    ) -> *mut Self::pcre2_code;
    unsafe fn pcre2_pattern_info(
        arg1: *const Self::pcre2_code,
        arg2: u32,
        arg3: *mut ::libc::c_void,
    ) -> ::libc::c_int;

    unsafe fn pcre2_match(
        arg1: *const Self::pcre2_code,
        arg2: Self::PCRE2_SPTR,
        arg3: usize,
        arg4: usize,
        arg5: u32,
        arg6: *mut Self::pcre2_match_data,
        arg7: *mut Self::pcre2_match_context,
    ) -> ::libc::c_int;

    unsafe fn pcre2_jit_stack_create(
        arg1: ::libc::size_t,
        arg2: ::libc::size_t,
    ) -> *mut Self::pcre2_jit_stack;
    unsafe fn pcre2_jit_compile(
        arg1: *mut Self::pcre2_code,
        arg2: u32,
    ) -> ::libc::c_int;
    unsafe fn pcre2_jit_stack_assign(
        arg1: *mut Self::pcre2_match_context,
        arg3: *mut ::libc::c_void,
    );
    unsafe fn pcre2_jit_stack_free(arg1: *mut Self::pcre2_jit_stack);

    unsafe fn pcre2_compile_context_create() -> *mut Self::pcre2_compile_context;
    unsafe fn pcre2_set_newline(
        arg1: *mut Self::pcre2_compile_context,
        arg2: u32,
    ) -> ::libc::c_int;
    unsafe fn pcre2_compile_context_free(
        arg1: *mut Self::pcre2_compile_context,
    );

    unsafe fn pcre2_match_context_create() -> *mut Self::pcre2_match_context;
    unsafe fn pcre2_match_context_free(arg1: *mut Self::pcre2_match_context);

    unsafe fn pcre2_match_data_create_from_pattern(
        arg1: *const Self::pcre2_code,
    ) -> *mut Self::pcre2_match_data;
    unsafe fn pcre2_match_data_free(arg1: *mut Self::pcre2_match_data);

    unsafe fn pcre2_get_ovector_pointer(
        arg1: *mut Self::pcre2_match_data,
    ) -> *mut usize;
    unsafe fn pcre2_get_ovector_count(
        arg1: *mut Self::pcre2_match_data,
    ) -> u32;
}

#[derive(Debug)]
pub struct CodeUnitWidth8;
impl CodeUnitWidth for CodeUnitWidth8 {
    type pcre2_code = pcre2_code_8;
    type PCRE2_CHAR = PCRE2_UCHAR8;
    type PCRE2_SPTR = PCRE2_SPTR8;
    type pcre2_compile_context = pcre2_compile_context_8;
    type pcre2_match_context = pcre2_match_context_8;
    type pcre2_match_data = pcre2_match_data_8;
    type pcre2_jit_stack = pcre2_jit_stack_8;
    type name_table_entry = name_table_entry_8;
    type SubjectChar = u8;
    type Pattern = String;

    fn escape_subject(subject: &[Self::SubjectChar]) -> String {
        use std::ascii::escape_default;
        // Escape bytes.
        let mut s = String::new();
        for &b in subject {
            let escaped = escape_default(b).collect::<Vec<_>>();
            s.push_str(&String::from_utf8_lossy(&escaped));
        }
        s
    }

    fn pattern_to_sptr_len(
        pattern: &Self::Pattern,
    ) -> (Self::PCRE2_SPTR, usize) {
        (pattern.as_ptr(), pattern.len())
    }

    fn subject_to_sptr_len(
        subject: &[Self::SubjectChar],
    ) -> (Self::PCRE2_SPTR, usize) {
        (subject.as_ptr(), subject.len())
    }

    unsafe fn pcre2_config(
        arg1: u32,
        arg2: *mut ::libc::c_void,
    ) -> ::libc::c_int {
        pcre2_config_8(arg1, arg2)
    }
    unsafe fn pcre2_code_free(arg1: *mut Self::pcre2_code) {
        pcre2_code_free_8(arg1)
    }
    unsafe fn pcre2_compile(
        arg1: Self::PCRE2_SPTR,
        arg2: usize,
        arg3: u32,
        arg4: *mut ::libc::c_int,
        arg5: *mut ::libc::size_t,
        arg6: *mut Self::pcre2_compile_context,
    ) -> *mut Self::pcre2_code {
        pcre2_compile_8(arg1, arg2, arg3, arg4, arg5, arg6)
    }

    unsafe fn pcre2_jit_stack_create(
        arg1: ::libc::size_t,
        arg2: ::libc::size_t,
    ) -> *mut Self::pcre2_jit_stack {
        pcre2_jit_stack_create_8(arg1, arg2, ptr::null_mut())
    }
    unsafe fn pcre2_jit_compile(
        arg1: *mut Self::pcre2_code,
        arg2: u32,
    ) -> ::libc::c_int {
        pcre2_jit_compile_8(arg1, arg2)
    }
    unsafe fn pcre2_jit_stack_assign(
        arg1: *mut Self::pcre2_match_context,
        arg3: *mut ::libc::c_void,
    ) {
        pcre2_jit_stack_assign_8(arg1, None, arg3)
    }
    unsafe fn pcre2_jit_stack_free(arg1: *mut Self::pcre2_jit_stack) {
        pcre2_jit_stack_free_8(arg1)
    }

    unsafe fn pcre2_pattern_info(
        arg1: *const Self::pcre2_code,
        arg2: u32,
        arg3: *mut ::libc::c_void,
    ) -> ::libc::c_int {
        pcre2_pattern_info_8(arg1, arg2, arg3)
    }

    unsafe fn pcre2_match(
        arg1: *const Self::pcre2_code,
        arg2: Self::PCRE2_SPTR,
        arg3: usize,
        arg4: usize,
        arg5: u32,
        arg6: *mut Self::pcre2_match_data,
        arg7: *mut Self::pcre2_match_context,
    ) -> ::libc::c_int {
        pcre2_match_8(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
    }

    unsafe fn pcre2_compile_context_create() -> *mut Self::pcre2_compile_context
    {
        pcre2_compile_context_create_8(ptr::null_mut())
    }
    unsafe fn pcre2_match_context_free(arg1: *mut Self::pcre2_match_context) {
        pcre2_match_context_free_8(arg1)
    }

    unsafe fn pcre2_set_newline(
        arg1: *mut Self::pcre2_compile_context,
        arg2: u32,
    ) -> ::libc::c_int {
        pcre2_set_newline_8(arg1, arg2)
    }
    unsafe fn pcre2_compile_context_free(
        arg1: *mut Self::pcre2_compile_context,
    ) {
        pcre2_compile_context_free_8(arg1)
    }
    unsafe fn pcre2_match_context_create() -> *mut Self::pcre2_match_context {
        pcre2_match_context_create_8(ptr::null_mut())
    }

    unsafe fn pcre2_match_data_create_from_pattern(
        arg1: *const Self::pcre2_code,
    ) -> *mut Self::pcre2_match_data {
        pcre2_match_data_create_from_pattern_8(arg1, ptr::null_mut())
    }
    unsafe fn pcre2_match_data_free(arg1: *mut Self::pcre2_match_data) {
        pcre2_match_data_free_8(arg1)
    }

    unsafe fn pcre2_get_ovector_pointer(
        arg1: *mut Self::pcre2_match_data,
    ) -> *mut usize {
        pcre2_get_ovector_pointer_8(arg1)
    }
    unsafe fn pcre2_get_ovector_count(
        arg1: *mut Self::pcre2_match_data,
    ) -> u32 {
        pcre2_get_ovector_count_8(arg1)
    }
}

#[derive(Debug)]
pub struct CodeUnitWidth32;
impl CodeUnitWidth for CodeUnitWidth32 {
    type pcre2_code = pcre2_code_32;
    type PCRE2_CHAR = PCRE2_UCHAR32;
    type PCRE2_SPTR = PCRE2_SPTR32;
    type pcre2_compile_context = pcre2_compile_context_32;
    type pcre2_match_context = pcre2_match_context_32;
    type pcre2_match_data = pcre2_match_data_32;
    type pcre2_jit_stack = pcre2_jit_stack_32;
    type name_table_entry = name_table_entry_32;
    type SubjectChar = char;
    type Pattern = Box<[char]>;

    fn escape_subject(subject: &[Self::SubjectChar]) -> String {
        use std::ascii::escape_default;
        // Escape bytes.
        let mut s = String::new();
        for &c in subject {
            let mut bytes = [0; 4];
            for &b in c.encode_utf8(&mut bytes).as_bytes() {
                // Escape the byte.
                let escaped = escape_default(b).collect::<Vec<_>>();
                s.push_str(&String::from_utf8_lossy(&escaped));
            }
        }
        s
    }

    fn pattern_to_sptr_len(
        pattern: &Self::Pattern,
    ) -> (Self::PCRE2_SPTR, usize) {
        (pattern.as_ptr() as *const u32, pattern.len())
    }

    fn subject_to_sptr_len(
        subject: &[Self::SubjectChar],
    ) -> (Self::PCRE2_SPTR, usize) {
        (subject.as_ptr() as *const u32, subject.len())
    }

    unsafe fn pcre2_config(
        arg1: u32,
        arg2: *mut ::libc::c_void,
    ) -> ::libc::c_int {
        pcre2_config_32(arg1, arg2)
    }
    unsafe fn pcre2_code_free(arg1: *mut Self::pcre2_code) {
        pcre2_code_free_32(arg1)
    }
    unsafe fn pcre2_compile(
        arg1: Self::PCRE2_SPTR,
        arg2: usize,
        arg3: u32,
        arg4: *mut ::libc::c_int,
        arg5: *mut ::libc::size_t,
        arg6: *mut Self::pcre2_compile_context,
    ) -> *mut Self::pcre2_code {
        pcre2_compile_32(arg1, arg2, arg3, arg4, arg5, arg6)
    }

    unsafe fn pcre2_jit_stack_create(
        arg1: ::libc::size_t,
        arg2: ::libc::size_t,
    ) -> *mut Self::pcre2_jit_stack {
        pcre2_jit_stack_create_32(arg1, arg2, ptr::null_mut())
    }
    unsafe fn pcre2_jit_compile(
        arg1: *mut Self::pcre2_code,
        arg2: u32,
    ) -> ::libc::c_int {
        pcre2_jit_compile_32(arg1, arg2)
    }
    unsafe fn pcre2_jit_stack_assign(
        arg1: *mut Self::pcre2_match_context,
        arg3: *mut ::libc::c_void,
    ) {
        pcre2_jit_stack_assign_32(arg1, None, arg3)
    }
    unsafe fn pcre2_jit_stack_free(arg1: *mut Self::pcre2_jit_stack) {
        pcre2_jit_stack_free_32(arg1)
    }

    unsafe fn pcre2_pattern_info(
        arg1: *const Self::pcre2_code,
        arg2: u32,
        arg3: *mut ::libc::c_void,
    ) -> ::libc::c_int {
        pcre2_pattern_info_32(arg1, arg2, arg3)
    }

    unsafe fn pcre2_match(
        arg1: *const Self::pcre2_code,
        arg2: Self::PCRE2_SPTR,
        arg3: usize,
        arg4: usize,
        arg5: u32,
        arg6: *mut Self::pcre2_match_data,
        arg7: *mut Self::pcre2_match_context,
    ) -> ::libc::c_int {
        pcre2_match_32(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
    }

    unsafe fn pcre2_compile_context_create() -> *mut Self::pcre2_compile_context
    {
        pcre2_compile_context_create_32(ptr::null_mut())
    }
    unsafe fn pcre2_match_context_free(arg1: *mut Self::pcre2_match_context) {
        pcre2_match_context_free_32(arg1)
    }

    unsafe fn pcre2_set_newline(
        arg1: *mut Self::pcre2_compile_context,
        arg2: u32,
    ) -> ::libc::c_int {
        pcre2_set_newline_32(arg1, arg2)
    }
    unsafe fn pcre2_compile_context_free(
        arg1: *mut Self::pcre2_compile_context,
    ) {
        pcre2_compile_context_free_32(arg1)
    }
    unsafe fn pcre2_match_context_create() -> *mut Self::pcre2_match_context {
        pcre2_match_context_create_32(ptr::null_mut())
    }

    unsafe fn pcre2_match_data_create_from_pattern(
        arg1: *const Self::pcre2_code,
    ) -> *mut Self::pcre2_match_data {
        pcre2_match_data_create_from_pattern_32(arg1, ptr::null_mut())
    }
    unsafe fn pcre2_match_data_free(arg1: *mut Self::pcre2_match_data) {
        pcre2_match_data_free_32(arg1)
    }

    unsafe fn pcre2_get_ovector_pointer(
        arg1: *mut Self::pcre2_match_data,
    ) -> *mut usize {
        pcre2_get_ovector_pointer_32(arg1)
    }
    unsafe fn pcre2_get_ovector_count(
        arg1: *mut Self::pcre2_match_data,
    ) -> u32 {
        pcre2_get_ovector_count_32(arg1)
    }
}

/// Returns true if and only if PCRE2 believes that JIT is available.
pub fn is_jit_available() -> bool {
    type W = CodeUnitWidth8;
    let mut rc: u32 = 0;
    let error_code = unsafe {
        W::pcre2_config(PCRE2_CONFIG_JIT, &mut rc as *mut _ as *mut c_void)
    };
    if error_code < 0 {
        // If PCRE2_CONFIG_JIT is a bad option, then there's a bug somewhere.
        panic!("BUG: {}", Error::jit(error_code));
    }
    rc == 1
}

/// Returns the version of PCRE2 being used.
///
/// The tuple returned corresponds to the major and minor version, e.g.,
/// `(10, 32)`.
pub fn version() -> (u32, u32) {
    (PCRE2_MAJOR, PCRE2_MINOR)
}

/// Escapes all regular expression meta characters in `pattern`.
///
/// The string returned may be safely used as a literal in a regular
/// expression.
pub fn escape(pattern: &str) -> String {
    fn is_meta_character(c: char) -> bool {
        match c {
            '\\' | '.' | '+' | '*' | '?' | '(' | ')' | '|' | '[' | ']'
            | '{' | '}' | '^' | '$' | '#' | '-' => true,
            _ => false,
        }
    }

    // Is it really true that PCRE2 doesn't have an API routine to
    // escape a pattern so that it matches literally? Wow. I couldn't
    // find one. It does of course have \Q...\E, but, umm, what if the
    // literal contains a \E?
    let mut quoted = String::new();
    quoted.reserve(pattern.len());
    for c in pattern.chars() {
        if is_meta_character(c) {
            quoted.push('\\');
        }
        quoted.push(c);
    }
    quoted
}

/// A low level representation of a compiled PCRE2 code object.
pub(crate) struct Code<W: CodeUnitWidth> {
    code: *mut W::pcre2_code,
    compiled_jit: bool,
    // We hang on to this but don't use it so that it gets freed when the
    // compiled code gets freed. It's not clear whether this is necessary or
    // not, but presumably doesn't cost us much to be conservative.
    #[allow(dead_code)]
    ctx: CompileContext<W>,
}

// SAFETY: Compiled PCRE2 code objects are immutable once built and explicitly
// safe to use from multiple threads simultaneously.
//
// One hitch here is that JIT compiling can write into a PCRE2 code object, but
// we only ever JIT compile immediately after first building the code object
// and before making it available to the caller.
unsafe impl<W: CodeUnitWidth> Send for Code<W> {}
unsafe impl<W: CodeUnitWidth> Sync for Code<W> {}

impl<W: CodeUnitWidth> Drop for Code<W> {
    fn drop(&mut self) {
        unsafe { W::pcre2_code_free(self.code) }
    }
}

impl<W: CodeUnitWidth> Code<W> {
    /// Compile the given pattern with the given options. If there was a
    /// problem compiling the pattern, then return an error.
    pub(crate) fn new(
        pattern: &W::Pattern,
        options: u32,
        mut ctx: CompileContext<W>,
    ) -> Result<Self, Error> {
        let (mut error_code, mut error_offset) = (0, 0);
        let (pat_sptr, pat_len) = W::pattern_to_sptr_len(pattern);

        let code = unsafe {
            W::pcre2_compile(
                pat_sptr,
                pat_len,
                options,
                &mut error_code,
                &mut error_offset,
                ctx.as_mut_ptr(),
            )
        };
        if code.is_null() {
            Err(Error::compile(error_code, error_offset))
        } else {
            Ok(Code { code, compiled_jit: false, ctx })
        }
    }

    /// JIT compile this code object.
    ///
    /// If there was a problem performing JIT compilation, then this returns
    /// an error.
    pub(crate) fn jit_compile(&mut self) -> Result<(), Error> {
        let error_code =
            unsafe { W::pcre2_jit_compile(self.code, PCRE2_JIT_COMPLETE) };
        if error_code == 0 {
            self.compiled_jit = true;
            Ok(())
        } else {
            Err(Error::jit(error_code))
        }
    }

    /// Build and return an ordered sequence of all capture group names in this
    /// compiled regex.
    ///
    /// The returned vector has a slot for every capturing group (including the
    /// one corresponding to the entire regex, which is always unnamed). Groups
    /// that are unnamed are set to `None`.
    ///
    /// If there was a problem querying the compiled object for information,
    /// then this returns an error.
    pub(crate) fn capture_names(&self) -> Result<Vec<Option<String>>, Error> {
        // This is an object lesson in why C sucks. All we need is a map from
        // a name to a number, but we need to go through all sorts of
        // shenanigans to get it. In order to verify this code, see
        // https://www.pcre.org/current/doc/html/pcre2api.html
        // and search for PCRE2_INFO_NAMETABLE.

        let name_count = self.name_count()?;
        let name_entry_size_in_bytes =
            self.name_entry_size()? * std::mem::size_of::<W::PCRE2_CHAR>();
        let name_table = self.raw_name_table()?;
        let mut names = vec![None; self.capture_count()?];
        for i in 0..name_count {
            let entry = unsafe {
                name_table
                    .cast::<u8>()
                    .add(i * name_entry_size_in_bytes)
                    .cast::<W::name_table_entry>()
                    .as_ref()
                    .unwrap()
            };
            names[entry.index()] = Some(entry.name());
        }
        Ok(names)
    }

    /// Return the underlying raw pointer to the code object.
    pub(crate) fn as_ptr(&self) -> *const W::pcre2_code {
        self.code
    }

    /// Returns a pointer to the array of name table entries.
    ///
    /// Entries are in alphabetical order.
    fn raw_name_table(&self) -> Result<*const W::name_table_entry, Error> {
        let mut table: *const W::name_table_entry = ptr::null();
        let rc = unsafe {
            W::pcre2_pattern_info(
                self.as_ptr(),
                PCRE2_INFO_NAMETABLE,
                &mut table as *mut *const W::name_table_entry as *mut c_void,
            )
        };
        if rc != 0 {
            Err(Error::info(rc))
        } else {
            Ok(table)
        }
    }

    /// Returns the number of named capturing groups.
    fn name_count(&self) -> Result<usize, Error> {
        let mut count: u32 = 0;
        let rc = unsafe {
            W::pcre2_pattern_info(
                self.as_ptr(),
                PCRE2_INFO_NAMECOUNT,
                &mut count as *mut u32 as *mut c_void,
            )
        };
        if rc != 0 {
            Err(Error::info(rc))
        } else {
            Ok(count as usize)
        }
    }

    /// Returns the entry size of each name in the name table, in code units.
    fn name_entry_size(&self) -> Result<usize, Error> {
        let mut size: u32 = 0;
        let rc = unsafe {
            W::pcre2_pattern_info(
                self.as_ptr(),
                PCRE2_INFO_NAMEENTRYSIZE,
                &mut size as *mut u32 as *mut c_void,
            )
        };
        if rc != 0 {
            Err(Error::info(rc))
        } else {
            Ok(size as usize)
        }
    }

    /// Returns the total number of capturing groups in this regex. This
    /// includes the capturing group for the entire pattern, so that this is
    /// always 1 more than the number of syntactic groups in the pattern.
    pub(crate) fn capture_count(&self) -> Result<usize, Error> {
        let mut count: u32 = 0;
        let rc = unsafe {
            W::pcre2_pattern_info(
                self.as_ptr(),
                PCRE2_INFO_CAPTURECOUNT,
                &mut count as *mut u32 as *mut c_void,
            )
        };
        if rc != 0 {
            Err(Error::info(rc))
        } else {
            Ok(1 + count as usize)
        }
    }
}

/// A low level representation of PCRE2's compilation context.
pub(crate) struct CompileContext<W: CodeUnitWidth>(
    *mut W::pcre2_compile_context,
);

// SAFETY: Compile contexts are safe to read from multiple threads
// simultaneously. No interior mutability is used, so Sync is safe.
unsafe impl<W: CodeUnitWidth> Send for CompileContext<W> {}
unsafe impl<W: CodeUnitWidth> Sync for CompileContext<W> {}

impl<W: CodeUnitWidth> Drop for CompileContext<W> {
    fn drop(&mut self) {
        unsafe { W::pcre2_compile_context_free(self.0) }
    }
}

impl<W: CodeUnitWidth> CompileContext<W> {
    /// Create a new empty compilation context.
    ///
    /// If memory could not be allocated for the context, then this panics.
    pub(crate) fn new() -> Self {
        let ctx = unsafe { W::pcre2_compile_context_create() };
        assert!(!ctx.is_null(), "could not allocate compile context");
        CompileContext(ctx)
    }

    /// Set the PCRE2 newline sequence.
    ///
    /// Valid values are: PCRE2_NEWLINE_CR, PCRE2_NEWLINE_LF,
    /// PCRE2_NEWLINE_CRLF, PCRE2_NEWLINE_ANYCRLF, PCRE2_NEWLINE_ANY or
    /// PCRE2_NEWLINE_NUL. Using any other value results in an error.
    pub(crate) fn set_newline(&mut self, value: u32) -> Result<(), Error> {
        let rc = unsafe { W::pcre2_set_newline(self.0, value) };
        if rc == 0 {
            Ok(())
        } else {
            Err(Error::option(rc))
        }
    }

    fn as_mut_ptr(&mut self) -> *mut W::pcre2_compile_context {
        self.0
    }
}

/// Configuration for PCRE2's match context.
#[derive(Clone, Debug)]
pub(crate) struct MatchConfig {
    /// When set, a custom JIT stack will be created with the given maximum
    /// size.
    pub(crate) max_jit_stack_size: Option<usize>,
}

impl Default for MatchConfig {
    fn default() -> MatchConfig {
        MatchConfig { max_jit_stack_size: None }
    }
}

/// A low level representation of a match data block.
///
/// Technically, a single match data block can be used with multiple regexes
/// (not simultaneously), but in practice, we just create a single match data
/// block for each regex for each thread it's used in.
pub(crate) struct MatchData<W: CodeUnitWidth> {
    config: MatchConfig,
    match_context: *mut W::pcre2_match_context,
    match_data: *mut W::pcre2_match_data,
    jit_stack: Option<*mut W::pcre2_jit_stack>,
    ovector_ptr: *const usize,
    ovector_count: u32,
    _marker: PhantomData<W>,
}

// SAFETY: Match data blocks can be freely sent from one thread to another,
// but they do not support multiple threads using them simultaneously. We still
// implement Sync however, since we require mutable access to use the match
// data block for executing a search, which statically prevents simultaneous
// reading/writing. It is legal to read match data blocks from multiple threads
// simultaneously.
unsafe impl<W: CodeUnitWidth> Send for MatchData<W> {}
unsafe impl<W: CodeUnitWidth> Sync for MatchData<W> {}

impl<W: CodeUnitWidth> Drop for MatchData<W> {
    fn drop(&mut self) {
        unsafe {
            if let Some(stack) = self.jit_stack {
                W::pcre2_jit_stack_free(stack);
            }
            W::pcre2_match_data_free(self.match_data);
            W::pcre2_match_context_free(self.match_context);
        }
    }
}

impl<W: CodeUnitWidth> MatchData<W> {
    /// Create a new match data block from a compiled PCRE2 code object.
    ///
    /// This panics if memory could not be allocated for the block.
    pub(crate) fn new(config: MatchConfig, code: &Code<W>) -> MatchData<W> {
        let match_context = unsafe { W::pcre2_match_context_create() };
        assert!(!match_context.is_null(), "failed to allocate match context");

        let match_data =
            unsafe { W::pcre2_match_data_create_from_pattern(code.as_ptr()) };
        assert!(!match_data.is_null(), "failed to allocate match data block");

        let jit_stack = match config.max_jit_stack_size {
            None => None,
            Some(_) if !code.compiled_jit => None,
            Some(max) => {
                let stack = unsafe {
                    W::pcre2_jit_stack_create(cmp::min(max, 32 * 1 << 10), max)
                };
                assert!(!stack.is_null(), "failed to allocate JIT stack");

                unsafe {
                    W::pcre2_jit_stack_assign(
                        match_context,
                        stack as *mut c_void,
                    )
                };
                Some(stack)
            }
        };

        let ovector_ptr = unsafe { W::pcre2_get_ovector_pointer(match_data) };
        assert!(!ovector_ptr.is_null(), "got NULL ovector pointer");
        let ovector_count = unsafe { W::pcre2_get_ovector_count(match_data) };
        MatchData {
            config,
            match_context,
            match_data,
            jit_stack,
            ovector_ptr,
            ovector_count,
            _marker: PhantomData,
        }
    }

    /// Return the configuration for this match data object.
    pub(crate) fn config(&self) -> &MatchConfig {
        &self.config
    }

    /// Execute PCRE2's primary match routine on the given subject string
    /// starting at the given offset. The provided options are passed to PCRE2
    /// as is.
    ///
    /// This returns false if no match occurred.
    ///
    /// Match offsets can be extracted via `ovector`.
    ///
    /// # Safety
    ///
    /// This routine is marked unsafe because it allows the caller to set
    /// arbitrary PCRE2 options. Some of those options can invoke undefined
    /// behavior when not used correctly. For example, if PCRE2_NO_UTF_CHECK
    /// is given and UTF mode is enabled and the given subject string is not
    /// valid UTF-8, then the result is undefined.
    pub(crate) unsafe fn find(
        &mut self,
        code: &Code<W>,
        mut subject: &[W::SubjectChar],
        start: usize,
        options: u32,
    ) -> Result<bool, Error> {
        // When the subject is empty, we use an empty slice
        // with a known valid pointer. Otherwise, slices derived
        // from, e.g., an empty `Vec<u8>` may not have a valid
        // pointer, since creating an empty `Vec` is guaranteed
        // to not allocate.
        if subject.is_empty() {
            subject = &[];
        }
        let (subj_ptr, subj_len) = W::subject_to_sptr_len(subject);

        let rc = W::pcre2_match(
            code.as_ptr(),
            subj_ptr,
            subj_len,
            start,
            options,
            self.match_data,
            self.match_context,
        );
        if rc == PCRE2_ERROR_NOMATCH {
            Ok(false)
        } else if rc > 0 {
            Ok(true)
        } else {
            // We always create match data with
            // pcre2_match_data_create_from_pattern, so the ovector should
            // always be big enough.
            assert!(rc != 0, "ovector should never be too small");
            Err(Error::matching(rc))
        }
    }

    /// Return the ovector corresponding to this match data.
    ///
    /// The ovector represents match offsets as pairs. This always returns
    /// N + 1 pairs (so 2*N + 1 offsets), where N is the number of capturing
    /// groups in the original regex.
    pub(crate) fn ovector(&self) -> &[usize] {
        // SAFETY: Both our ovector pointer and count are derived directly from
        // the creation of a valid match data block. One interesting question
        // here is whether the contents of the ovector are always initialized.
        // The PCRE2 documentation suggests that they are (so does testing),
        // but this isn't actually 100% clear!
        unsafe {
            slice::from_raw_parts(
                self.ovector_ptr,
                // This could in theory overflow, but the ovector count comes
                // directly from PCRE2, so presumably it's guaranteed to never
                // overflow size_t/usize. Also, in practice, this would require
                // a number of capture groups so large as to be probably
                // impossible.
                self.ovector_count as usize * 2,
            )
        }
    }
}
