bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, Default, PartialEq)]
    pub struct ScopeFlags: u8 {
      const FUNCTION = 1 << 0;
    }
}
