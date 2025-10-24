// #[derive(Debug, Clone, Copy, PartialEq)]
// pub enum F64RepresentKind {
//     Word8,
//     Word16,
//     Word32,
//     Word64,
//     Float16,
//     Float32,
//     Float64,
// }

#[derive(Debug, Clone, Copy)]
pub struct F64Represent {
    inner: u64,
    // kind: F64RepresentKind,
}

impl PartialEq for F64Represent {
    fn eq(&self, other: &Self) -> bool {
        // debug_assert!(
        //     !ret || self.kind == other.kind,
        //     "F64Represent should be equal if inner is equal, but got {self:?} and {other:?}"
        // );
        self.inner == other.inner
    }
}

impl Eq for F64Represent {}

impl std::hash::Hash for F64Represent {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

impl F64Represent {
    pub fn new(val: f64) -> Self {
        // const F16_MAX: f64 = 65504.0; // Maximum value for f16
        // const F16_MIN: f64 = -65504.0; // Minimum value for
        // let is_int = val.trunc() == val;
        // let kind = if is_int {
        //     if val <= u8::MAX as f64 && val >= u8::MIN as f64 {
        //         F64RepresentKind::Word8
        //     } else if val <= u16::MAX as f64 && val >= u16::MIN as f64 {
        //         F64RepresentKind::Word16
        //     } else if val <= u32::MAX as f64 && val >= u32::MIN as f64 {
        //         F64RepresentKind::Word32
        //     } else if val <= u64::MAX as f64 && val >= u64::MIN as f64 {
        //         F64RepresentKind::Word64
        //     } else {
        //         F64RepresentKind::Float64
        //     }
        // } else if (F16_MIN..=F16_MAX).contains(&val) {
        //     F64RepresentKind::Float16
        // } else if val <= f32::MAX as f64 && val >= f32::MIN as f64 {
        //     F64RepresentKind::Float32
        // } else {
        //     F64RepresentKind::Float64
        // };
        Self {
            inner: val.to_bits(),
            // kind,
        }
    }

    pub fn val(&self) -> f64 {
        f64::from_bits(self.inner)
    }

    pub fn neg(&self) -> Self {
        let negated = -self.val();
        F64Represent::new(negated)
    }
}

impl From<f64> for F64Represent {
    fn from(val: f64) -> Self {
        F64Represent::new(val)
    }
}

impl From<usize> for F64Represent {
    fn from(val: usize) -> Self {
        F64Represent::new(val as f64)
    }
}

impl From<F64Represent> for f64 {
    fn from(val: F64Represent) -> Self {
        f64::from_bits(val.inner)
    }
}

impl nohash_hasher::IsEnabled for F64Represent {}
