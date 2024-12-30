use super::ast;
use super::ElementFlags;
use super::TyMapper;
use crate::bind::SymbolID;
use crate::check::TyChecker;

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
    pub ty_params: Option<super::Tys<'cx>>,
    pub params: &'cx [SymbolID],
    pub min_args_count: usize,
    // TODO: remove `Option` and use ty `ty`
    pub ret: Option<ast::NodeID>,
    pub node_id: ast::NodeID,
    pub target: Option<&'cx Sig<'cx>>,
    pub mapper: Option<&'cx TyMapper<'cx>>,
}

impl<'cx> Sig<'cx> {
    pub const fn has_rest_param(&self) -> bool {
        self.flags.intersects(SigFlags::HAS_REST_PARAMETER)
    }

    pub fn get_rest_ty(&self, checker: &mut TyChecker<'cx>) -> Option<&'cx super::Ty<'cx>> {
        self.has_rest_param().then(|| {
            let rest_ty = checker.get_type_of_symbol(*self.params.last().unwrap());
            if !rest_ty.kind.is_tuple() {
                if rest_ty.kind.is_any() {
                    // TODO: any_array_ty
                    checker.any_ty()
                } else {
                    rest_ty
                }
            } else {
                // TODO: tuple
                checker.any_ty()
            }
        })
    }

    pub fn get_non_array_rest_ty(
        &self,
        checker: &mut TyChecker<'cx>,
    ) -> Option<&'cx super::Ty<'cx>> {
        self.get_rest_ty(checker).and_then(|ty| {
            if !ty.kind.is_array(checker) && !ty.kind.is_any() {
                Some(ty)
            } else {
                None
            }
        })
    }

    pub fn get_param_count(&self, checker: &mut TyChecker<'cx>) -> usize {
        let len = self.params.len();
        if self.has_rest_param() {
            let rest_ty = checker.get_type_of_symbol(self.params[len - 1]);
            if rest_ty.kind.is_tuple() {
                let tuple = rest_ty
                    .kind
                    .expect_object_reference()
                    .target
                    .kind
                    .expect_object_tuple();
                let var = if tuple.combined_flags.intersects(ElementFlags::VARIABLE) {
                    0
                } else {
                    1
                };
                return len + tuple.shape.fixed_length - var;
            }
        }
        len
    }
}

pub type Sigs<'cx> = &'cx [&'cx Sig<'cx>];

#[derive(Debug, Clone, Copy)]
pub enum SigKind {
    Call,
    Constructor,
}
