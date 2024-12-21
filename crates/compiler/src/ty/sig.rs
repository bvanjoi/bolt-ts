use super::ast;
use crate::bind::SymbolID;

bitflags::bitflags! {
  #[derive(Debug, Clone, Copy)]
  pub struct SigFlags: u8 {
      const HAS_REST_PARAMETER  = 1 << 0;
      const HAS_ABSTRACT = 1 << 2;
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Sig<'cx> {
    pub flags: SigFlags,
    pub ty_params: Option<&'cx [SymbolID]>,
    pub params: &'cx [SymbolID],
    pub min_args_count: usize,
    // TODO: remove `Option` and use ty `ty`
    pub ret: Option<ast::NodeID>,
    pub node_id: ast::NodeID,
}

impl Sig<'_> {
    pub fn has_rest_param(&self) -> bool {
        self.flags.intersects(SigFlags::HAS_REST_PARAMETER)
    }
}

pub type Sigs<'cx> = &'cx [&'cx Sig<'cx>];
