use bolt_ts_ecma_logical::{JS_MAX_INT, JS_MAX_SAFE_INTEGER, JS_MIN_INT, JS_MIN_SAFE_INTEGER};

struct Double(u64);

impl From<f64> for Double {
    fn from(value: f64) -> Self {
        Double(value.to_bits())
    }
}

impl Double {
    const SIGN_MASK: u64 = 0x8000_0000_0000_0000;
    const EXP_MASK: u64 = 0x7FF0_0000_0000_0000;
    const SIGNIFICAND_MASK: u64 = 0x000F_FFFF_FFFF_FFFF;
    const HIDDEN_BIT: u64 = 0x0010_0000_0000_0000;
    const PHYSICAL_SIGNIFICAND_SIZE: u64 = 52;
    const SIGNIFICAND_SIZE: u64 = 53;

    const EXP_BIAS: i32 = 1023 + Self::PHYSICAL_SIGNIFICAND_SIZE as i32;
    const DENORMAL_EXP: i32 = -1 * Self::EXP_BIAS + 1;

    #[inline(always)]
    const fn is_denormal(&self) -> bool {
        (self.0 & Self::EXP_MASK) == 0
    }

    #[inline(always)]
    const fn exp(&self) -> i32 {
        if self.is_denormal() {
            Self::DENORMAL_EXP
        } else {
            (((self.0 & Self::EXP_MASK) >> Self::PHYSICAL_SIGNIFICAND_SIZE) as i32) - Self::EXP_BIAS
        }
    }

    const fn significand(&self) -> u64 {
        let man = self.0 & Self::SIGNIFICAND_MASK;
        if !self.is_denormal() {
            man + Self::HIDDEN_BIT
        } else {
            man
        }
    }

    const fn sign(&self) -> i64 {
        if self.0 & Self::SIGN_MASK == 0 { 1 } else { -1 }
    }
}

/// reference: [ToInt32](https://tc39.es/ecma262/#sec-toint32)
pub fn js_double_to_int32(f: f64) -> i32 {
    const I32_MAX: f64 = i32::MAX as f64;
    const I32_MIN: f64 = i32::MIN as f64;

    if f.is_finite() && f >= I32_MIN && f <= I32_MAX {
        return f as i32;
    }

    let d = Double::from(f);
    let exp = d.exp();
    const BOUND: i32 = -1 * Double::SIGNIFICAND_SIZE as i32;
    if exp <= BOUND || exp > 31 {
        return 0;
    }

    let bits = if exp < 0 {
        d.significand() >> (-exp)
    } else {
        (d.significand() << exp) & 0xFFFF_FFFF
    };

    (d.sign() * (bits as i64)) as i32
}

#[test]
fn test_js_double_to_int32() {
    assert_eq!(js_double_to_int32(0.0), 0);
    assert_eq!(js_double_to_int32(-0.0), 0);
    assert_eq!(js_double_to_int32(f64::NAN), 0);
    assert_eq!(js_double_to_int32(f64::INFINITY), 0);
    assert_eq!(js_double_to_int32(f64::NEG_INFINITY), 0);
    assert_eq!(js_double_to_int32(3.1415926), 3);
    assert_eq!(js_double_to_int32(-3.1415926), -3);
    assert_eq!(js_double_to_int32(1.9999999), 1);
    assert_eq!(js_double_to_int32(-1.9999999), -1);
    assert_eq!(js_double_to_int32(JS_MAX_INT as f64), JS_MAX_INT);
    assert_eq!(js_double_to_int32(JS_MIN_INT as f64), JS_MIN_INT);
    assert_eq!(js_double_to_int32(2147483648.0), -2147483648);
    assert_eq!(js_double_to_int32(2147483647.0), 2147483647);
    assert_eq!(js_double_to_int32(-2147483648.0), -2147483648);
    assert_eq!(js_double_to_int32(JS_MAX_SAFE_INTEGER as f64), -1);
    assert_eq!(js_double_to_int32(JS_MAX_SAFE_INTEGER as f64 + 1.0), 0);
    assert_eq!(js_double_to_int32(JS_MIN_SAFE_INTEGER as f64), 1);
    assert_eq!(js_double_to_int32(JS_MIN_SAFE_INTEGER as f64 - 1.0), 0);
}
