bitflags::bitflags! {
  #[derive(Clone, Copy, Debug)]
  pub struct NodeFlags: u32 {
    const NONE  = 0;
    const LET   = 1 << 0;
    const CONST = 1 << 1;
    const USING = 1 << 2;
  }
}
