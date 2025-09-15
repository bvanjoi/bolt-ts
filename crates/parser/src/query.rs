#[derive(Clone, Copy, PartialEq)]
pub enum AccessKind {
    Read,
    Write,
    ReadWrite,
}
