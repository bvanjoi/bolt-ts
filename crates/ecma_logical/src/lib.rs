mod base;

use self::base::Double;

/// `2^53 - 1`
pub const JS_MAX_SAFE_INTEGER: i64 = 9007199254740991;
pub const JS_MIN_SAFE_INTEGER: i64 = -9007199254740991;
pub const JS_MIN_INT: i32 = 0x80000000_u32 as i32;
pub const JS_MAX_INT: i32 = 0x7FFFFFFF;

/// reference: [ToInt32](https://tc39.es/ecma262/#sec-toint32)
pub const fn js_double_to_int32(f: f64) -> i32 {
    const I32_MAX: f64 = i32::MAX as f64;
    const I32_MIN: f64 = i32::MIN as f64;

    if f.is_finite() && f >= I32_MIN && f <= I32_MAX {
        return f as i32;
    }

    let d = Double::from_f64(f);
    let exp = d.exp();
    const BOUND: i32 = -(Double::SIGNIFICAND_SIZE as i32);
    if exp <= BOUND || exp > 31 {
        return 0;
    }

    let bits = if exp < 0 {
        d.significand() >> -exp
    } else {
        (d.significand() << exp) & 0xFFFF_FFFF
    } as i64;

    (d.sign() * bits) as i32
}

/// reference: [ToBoolean](https://tc39.es/ecma262/#sec-toboolean)
pub const fn js_double_to_boolean(f: f64) -> bool {
    f != 0. && !f.is_nan()
}

/// reference: [ToUInt32](https://tc39.es/ecma262/#sec-toint32)
pub const fn js_double_to_uint32(f: f64) -> u32 {
    js_double_to_int32(f) as u32
}
