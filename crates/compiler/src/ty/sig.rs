use std::hash::Hash;

use super::ast;
use super::ElementFlags;
use super::TyMap;
use super::TypeFlags;
use crate::bind::SymbolID;
use crate::check::TyChecker;

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct SigFlags: u8 {
        const HAS_REST_PARAMETER = 1 << 0;
        const HAS_LITERAL_TYPES = 1 << 1;
        const ABSTRACT = 1 << 2;

        const IS_INNER_CALL_CHAIN = 1 << 3;
        const IS_OUTER_CALL_CHAIN = 1 << 4;
        const IS_UNTYPED_SIGNATURE_IN_JS_FILE = 1 << 5;
        const IS_NON_INFERRABLE = 1 << 6;
        const IS_SIGNATURE_CANDIDATE_FOR_OVERLOAD_FAILURE = 1 << 7;

        const PROPAGATING_FLAGS = Self::HAS_REST_PARAMETER.bits()
                                | Self::HAS_LITERAL_TYPES.bits()
                                | Self::ABSTRACT.bits()
                                | Self::IS_UNTYPED_SIGNATURE_IN_JS_FILE.bits()
                                | Self::IS_SIGNATURE_CANDIDATE_FOR_OVERLOAD_FAILURE.bits();

        const CALL_CHAIN_FLAGS = Self::IS_INNER_CALL_CHAIN.bits()
                            | Self::IS_OUTER_CALL_CHAIN.bits();
    }
}

bolt_ts_utils::index!(SigID);

impl nohash_hasher::IsEnabled for SigID {}

impl SigID {
    pub const fn dummy() -> Self {
        Self(u32::MAX)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Sig<'cx> {
    pub id: SigID,
    pub flags: SigFlags,
    pub ty_params: Option<super::Tys<'cx>>,
    pub params: &'cx [SymbolID],
    pub min_args_count: usize,
    pub ret: Option<ast::NodeID>,
    pub node_id: Option<ast::NodeID>,
    pub target: Option<&'cx Sig<'cx>>,
    pub mapper: Option<&'cx dyn TyMap<'cx>>,
    /// Occurs when this signature is generated by an implicit class constructor.
    pub class_decl: Option<ast::NodeID>,
}

impl<'cx> Sig<'cx> {
    pub fn with_id(mut self, id: usize) -> Self {
        assert!(self.id == SigID::dummy());
        self.id = SigID(id as u32);
        self
    }

    pub const fn has_rest_param(&self) -> bool {
        self.flags.intersects(SigFlags::HAS_REST_PARAMETER)
    }

    pub const fn has_literal_tys(&self) -> bool {
        self.flags.intersects(SigFlags::HAS_LITERAL_TYPES)
    }

    pub fn get_rest_ty(&self, checker: &mut TyChecker<'cx>) -> Option<&'cx super::Ty<'cx>> {
        self.has_rest_param().then(|| {
            let rest_ty = checker.get_type_of_symbol(*self.params.last().unwrap());
            if !rest_ty.is_tuple() {
                if rest_ty.flags.intersects(TypeFlags::ANY) {
                    checker.any_array_ty()
                } else {
                    rest_ty
                }
            } else {
                // TODO: tuple
                checker.any_ty
            }
        })
    }

    pub fn get_non_array_rest_ty(
        &self,
        checker: &mut TyChecker<'cx>,
    ) -> Option<&'cx super::Ty<'cx>> {
        self.get_rest_ty(checker).and_then(|ty| {
            if !ty.kind.is_array(checker) && !ty.flags.intersects(TypeFlags::ANY) {
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
            if rest_ty.is_tuple() {
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
                return len + tuple.fixed_length - var;
            }
        }
        len
    }

    pub fn def_id(&self) -> ast::NodeID {
        self.node_id.unwrap_or_else(|| self.class_decl.unwrap())
    }
}

pub type Sigs<'cx> = &'cx [&'cx Sig<'cx>];

impl PartialEq for Sig<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SigKind {
    Call,
    Constructor,
}
