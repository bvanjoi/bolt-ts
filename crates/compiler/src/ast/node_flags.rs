bitflags::bitflags! {
  #[derive(Clone, Copy, Debug)]
  pub struct NodeFlags: u32 {
    const AMBIENT = 1 << 25;
  }
}
