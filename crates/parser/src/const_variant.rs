pub const JS_VARIANT: u8 = 0b000;
pub const TS_VARIANT: u8 = 0b001;
pub const JSX_VARIANT: u8 = 0b010;
pub const TSX_VARIANT: u8 = 0b011;
pub const DTS_VARIANT: u8 = 0b100;
pub const PRESERVE_COMMENT: u8 = 0b1000;

const fn filter_language_variant(variant: u8) -> u8 {
    variant & 0x0007
}

const fn is_valid_language_variant(variant: u8) -> bool {
    matches!(
        filter_language_variant(variant),
        JS_VARIANT | TS_VARIANT | JSX_VARIANT | TSX_VARIANT | DTS_VARIANT
    )
}

pub const fn is_jsx_like_variant(variant: u8) -> bool {
    debug_assert!(is_valid_language_variant(variant));
    matches!(filter_language_variant(variant), JSX_VARIANT | TSX_VARIANT)
}

pub const fn is_js_variant(variant: u8) -> bool {
    debug_assert!(is_valid_language_variant(variant));
    matches!(filter_language_variant(variant), JS_VARIANT)
}

pub const fn is_ts_like_variant(variant: u8) -> bool {
    debug_assert!(is_valid_language_variant(variant));
    matches!(
        filter_language_variant(variant),
        TS_VARIANT | TSX_VARIANT | DTS_VARIANT
    )
}

pub const fn is_preserve_comment(variant: u8) -> bool {
    (variant & PRESERVE_COMMENT) != 0
}
