use bolt_ts_ecma_logical::JS_MAX_SAFE_INTEGER;

pub(super) fn parse_integer<const RADIX: u8>(s: &str) -> f64 {
    debug_assert!(matches!(RADIX, 2 | 8 | 10 | 16));
    debug_assert!(!s.is_empty());
    debug_assert!(!s.contains('_'));
    if RADIX == 10 {
        parse_integer_from_dec(s)
    } else if RADIX == 16 {
        parse_integer_from_hex(s)
    } else if RADIX == 2 {
        parse_integer_from_bin(s)
    } else if RADIX == 8 {
        parse_integer_from_oct(s)
    } else {
        unreachable!()
    }
}

fn parse_integer_from_hex(s: &str) -> f64 {
    debug_assert!(s.chars().all(|c| c.is_ascii_hexdigit()));
    const JS_MAX_FAST_INT_LEN: usize = JS_MAX_SAFE_INTEGER.ilog(16) as usize;
    if s.len() > JS_MAX_FAST_INT_LEN {
        s.as_bytes().iter().fold(0f64, |res, &cur| {
            res.mul_add(
                16.,
                if cur < b'a' {
                    cur - b'0'
                } else {
                    cur - b'a' + 10
                } as f64,
            )
        })
    } else {
        u64::from_str_radix(s, 16).unwrap() as f64
    }
}

fn parse_integer_from_bin(s: &str) -> f64 {
    debug_assert!(s.chars().all(|c| c == '0' || c == '1'));
    const JS_MAX_FAST_INT_LEN: usize = JS_MAX_SAFE_INTEGER.ilog2() as usize;
    if s.len() > JS_MAX_FAST_INT_LEN {
        s.as_bytes().iter().fold(0f64, |res, &cur| {
            res.mul_add(2., if cur == b'0' { 0. } else { 1. })
        })
    } else {
        u64::from_str_radix(s, 2).unwrap() as f64
    }
}

fn parse_integer_from_oct(s: &str) -> f64 {
    debug_assert!(s.chars().all(|c| matches!(c, '0'..='7')));
    const JS_MAX_FAST_INT_LEN: usize = JS_MAX_SAFE_INTEGER.ilog(8) as usize;
    if s.len() > JS_MAX_FAST_INT_LEN {
        s.as_bytes()
            .iter()
            .fold(0f64, |res, &cur| res.mul_add(8., (cur - b'0') as f64))
    } else {
        u64::from_str_radix(s, 8).unwrap() as f64
    }
}

fn parse_integer_from_dec(s: &str) -> f64 {
    debug_assert!(s.chars().all(|c| c.is_ascii_digit()));
    const JS_MAX_FAST_INT_LEN: usize = JS_MAX_SAFE_INTEGER.ilog10() as usize;
    if s.len() > JS_MAX_FAST_INT_LEN {
        s.parse::<f64>().unwrap()
    } else {
        s.parse::<u64>().unwrap() as f64
    }
}
