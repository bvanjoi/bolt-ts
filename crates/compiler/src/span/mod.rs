#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn new(lo: u32, hi: u32) -> Self {
        Self { lo, hi }
    }
}

impl From<(usize, usize)> for Span {
    fn from((lo, hi): (usize, usize)) -> Self {
        Self::new(lo as u32, hi as u32)
    }
}
