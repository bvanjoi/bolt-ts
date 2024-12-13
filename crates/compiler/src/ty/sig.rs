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
    pub params: &'cx [SymbolID],
    pub min_args_count: usize,
    // TODO: remove `Option`
    pub ret: Option<SymbolID>,
}

impl Sig<'_> {
    pub fn has_rest_param(&self) -> bool {
        self.flags.intersects(SigFlags::HAS_REST_PARAMETER)
    }
}

pub enum SigDecl<'cx> {
    FnDecl(&'cx ast::FnDecl<'cx>),
    ClassDecl(&'cx ast::ClassDecl<'cx>),
    FnExpr(&'cx ast::FnExpr<'cx>),
    ArrowFnExpr(&'cx ast::ArrowFnExpr<'cx>),
    ClassCtor(&'cx ast::ClassCtor<'cx>),
    CtorSigDecl(&'cx ast::CtorSigDecl<'cx>),
}

impl<'cx> SigDecl<'cx> {
    pub fn params(&self) -> ast::ParamsDecl<'cx> {
        match self {
            SigDecl::FnDecl(f) => f.params,
            SigDecl::ClassDecl(c) => &[],
            SigDecl::FnExpr(f) => f.params,
            SigDecl::ArrowFnExpr(f) => f.params,
            SigDecl::ClassCtor(f) => f.params,
            SigDecl::CtorSigDecl(f) => f.params,
        }
    }

    pub fn id(&self) -> ast::NodeID {
        match self {
            SigDecl::FnDecl(f) => f.id,
            SigDecl::ClassDecl(c) => c.id,
            SigDecl::FnExpr(f) => f.id,
            SigDecl::ArrowFnExpr(f) => f.id,
            SigDecl::ClassCtor(f) => f.id,
            SigDecl::CtorSigDecl(f) => f.id,
        }
    }

    pub fn has_rest_param(&self) -> bool {
        self.params()
            .last()
            .map_or(false, |param| param.dotdotdot.is_some())
    }
}
