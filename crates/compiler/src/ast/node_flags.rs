bitflags::bitflags! {
  #[derive(Clone, Copy, Debug)]
  pub struct NodeFlags: u8 {
    const LET   = 1 << 0;
    const CONST = 1 << 1;
    const USING = 1 << 2;
  }
}
