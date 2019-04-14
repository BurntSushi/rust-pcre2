use std::ptr;

use crate::bindings::*;

#[test]
fn itworks() {
    let mut error_code = 0;
    let mut error_offset = 0;
    let pattern = r"\w+";
    let code = unsafe {
        pcre2_compile_8(
            pattern.as_ptr(),
            pattern.len(),
            PCRE2_UCP | PCRE2_UTF,
            &mut error_code,
            &mut error_offset,
            ptr::null_mut(),
        )
    };
    if code.is_null() {
        panic!(
            "compilation failed; error code: {:?}, offset: {:?}",
            error_code,
            error_offset
        );
    }

    let match_data = unsafe {
        pcre2_match_data_create_from_pattern_8(code, ptr::null_mut())
    };
    if match_data.is_null() {
        unsafe {
            pcre2_code_free_8(code);
        }
        panic!("could not allocate match_data");
    }

    let ovector = unsafe { pcre2_get_ovector_pointer_8(match_data) };
    if ovector.is_null() {
        unsafe {
            pcre2_match_data_free_8(match_data);
            pcre2_code_free_8(code);
        }
        panic!("could not get ovector");
    }

    let subject = "  βabcβ foo";
    let rc = unsafe {
        pcre2_match_8(
            code,
            subject.as_ptr(),
            subject.len(),
            0,
            0,
            match_data,
            ptr::null_mut(),
        )
    };
    if rc <= 0 {
        unsafe {
            pcre2_match_data_free_8(match_data);
            pcre2_code_free_8(code);
        }
        panic!("error executing match");
    }

    let (s, e) = unsafe { (*ovector.offset(0), *ovector.offset(1)) };
    unsafe {
        pcre2_match_data_free_8(match_data);
        pcre2_code_free_8(code);
    }

    assert_eq!("βabcβ", &subject[s..e]);
}
