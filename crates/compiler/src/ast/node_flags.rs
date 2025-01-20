bitflags::bitflags! {
  #[derive(Clone, Copy, Debug, Default)]
  pub struct NodeFlags: u32 {
    const DECORATOR_CONTEXT   = 1 << 15;
    const DISALLOW_IN_CONTEXT = 1 << 17;
    const AMBIENT             = 1 << 25;

    const DISALLOW_IN_AND_DECORATOR_CONTEXT = Self::DISALLOW_IN_CONTEXT.bits() | Self::DECORATOR_CONTEXT.bits();
  }
}
