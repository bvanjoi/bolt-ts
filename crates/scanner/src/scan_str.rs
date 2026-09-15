use super::unicode::{is_unicode_es5_identifier_part, is_unicode_esnext_identifier_part};
use super::unicode::{is_unicode_es5_identifier_start, is_unicode_esnext_identifier_start};

#[inline(always)]
const fn is_ascii_letter(ch: u8) -> bool {
    ch.is_ascii_alphabetic()
}

#[inline(always)]
const fn is_word_character(ch: u8) -> bool {
    is_ascii_letter(ch) || ch.is_ascii_digit() || ch == b'_'
}

#[inline(always)]
pub const fn is_ascii_identifier_start(ch: u8) -> bool {
    ch == b'$' || ch == b'_' || is_ascii_letter(ch)
}

#[inline(always)]
pub fn is_non_ascii_identifier_start<const IS_ES5_TARGET: bool>(ch: u32) -> bool {
    debug_assert!(ch > 127);
    if IS_ES5_TARGET {
        is_unicode_es5_identifier_start(ch)
    } else {
        is_unicode_esnext_identifier_start(ch)
    }
}

#[inline(always)]
pub fn is_identifier_start<const IS_ES5_TARGET: bool>(ch: u32) -> bool {
    if ch <= 127 {
        is_ascii_identifier_start(ch as u8)
    } else {
        if IS_ES5_TARGET {
            is_non_ascii_identifier_start::<true>(ch)
        } else {
            is_non_ascii_identifier_start::<false>(ch)
        }
    }
}

#[inline(always)]
pub const fn is_ascii_identifier_part(ch: u8) -> bool {
    is_word_character(ch) || ch == b'$'
}

#[inline(always)]
pub fn is_identifier_part<const IS_ES5_TARGET: bool>(ch: u32) -> bool {
    if ch <= 127 {
        is_ascii_identifier_part(ch as u8)
    } else if IS_ES5_TARGET {
        is_unicode_es5_identifier_part(ch)
    } else {
        is_unicode_esnext_identifier_part(ch)
    }
}

pub mod non_ascii_character_code {
    pub const NON_BREAKING_SPACE: u32 = 0x00A0;
    pub const LINE_BREAK: u32 = 0x0085;
    pub const EN_QUAD: u32 = 0x2000;
    pub const EM_QUAD: u32 = 0x2001;
    pub const EN_SPACE: u32 = 0x2002;
    pub const EM_SPACE: u32 = 0x2003;
    pub const THREE_PER_EM_SPACE: u32 = 0x2004;
    pub const FOUR_PER_EM_SPACE: u32 = 0x2005;
    pub const SIX_PER_EM_SPACE: u32 = 0x2006;
    pub const FIGURE_SPACE: u32 = 0x2007;
    pub const PUNCTUATION_SPACE: u32 = 0x2008;
    pub const THIN_SPACE: u32 = 0x2009;
    pub const HAIR_SPACE: u32 = 0x200A;
    pub const ZERO_WIDTH_SPACE: u32 = 0x200B;
    pub const NARROW_NO_BREAK_SPACE: u32 = 0x202F;
    pub const IDEOGRAPHIC_SPACE: u32 = 0x3000;
    pub const MATHEMATICAL_SPACE: u32 = 0x205F;
    pub const OGHAM: u32 = 0x1680;
}

pub fn utf16_encode_as_bytes(code_point: u32) -> Vec<u8> {
    assert!(code_point <= 0x10FFFF);
    if code_point < 256 {
        return vec![code_point as u8];
    } else if code_point <= 0xFFFF {
        let lo = (code_point & 0xFF) as u8;
        let hi = ((code_point >> 8) & 0xFF) as u8;
        return vec![lo, hi];
    }

    let surrogate = code_point - 0x10000;
    let high_surrogate = ((surrogate >> 10) + 0xD800) as u16;
    let low_surrogate = ((surrogate & 0x3FF) + 0xDC00) as u16;

    let mut buf = Vec::with_capacity(4);
    buf.extend_from_slice(&high_surrogate.to_le_bytes());
    buf.extend_from_slice(&low_surrogate.to_le_bytes());
    buf
}

#[test]
fn test_utf16_encode_as_bytes() {
    assert_eq!(utf16_encode_as_bytes(9), vec![9]);
    assert_eq!(utf16_encode_as_bytes(20), vec![20]);
    assert_eq!(utf16_encode_as_bytes(255), vec![255]);
}
