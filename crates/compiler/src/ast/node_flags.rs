bitflags::bitflags! {
  #[derive(Clone, Copy, Debug)]
  pub struct NodeFlags: u32 {
    const DISALLOW_IN_CONTEXT = 1 << 17;
    const AMBIENT             = 1 << 25;
  }
}
