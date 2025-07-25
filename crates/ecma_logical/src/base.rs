pub(super) struct Double(u64);

impl Double {
    pub(super) const SIGN_MASK: u64 = 0x8000_0000_0000_0000;
    pub(super) const EXP_MASK: u64 = 0x7FF0_0000_0000_0000;
    pub(super) const SIGNIFICAND_MASK: u64 = 0x000F_FFFF_FFFF_FFFF;
    pub(super) const HIDDEN_BIT: u64 = 0x0010_0000_0000_0000;
    pub(super) const PHYSICAL_SIGNIFICAND_SIZE: u64 = 52;
    pub(super) const SIGNIFICAND_SIZE: u64 = 53;

    pub(super) const EXP_BIAS: i32 = 1023 + Self::PHYSICAL_SIGNIFICAND_SIZE as i32;
    pub(super) const DENORMAL_EXP: i32 = -Self::EXP_BIAS + 1;

    pub(super) const fn from_f64(value: f64) -> Self {
        Double(value.to_bits())
    }
    #[inline(always)]
    pub(super) const fn is_denormal(&self) -> bool {
        (self.0 & Self::EXP_MASK) == 0
    }

    #[inline(always)]
    pub(super) const fn exp(&self) -> i32 {
        if self.is_denormal() {
            Self::DENORMAL_EXP
        } else {
            (((self.0 & Self::EXP_MASK) >> Self::PHYSICAL_SIGNIFICAND_SIZE) as i32) - Self::EXP_BIAS
        }
    }

    #[inline(always)]
    pub(super) const fn significand(&self) -> u64 {
        let man = self.0 & Self::SIGNIFICAND_MASK;
        if !self.is_denormal() {
            man + Self::HIDDEN_BIT
        } else {
            man
        }
    }

    #[inline(always)]
    pub(super) const fn sign(&self) -> i64 {
        if self.0 & Self::SIGN_MASK == 0 { 1 } else { -1 }
    }
}
