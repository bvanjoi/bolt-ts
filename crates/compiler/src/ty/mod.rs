use crate::atoms::{AtomId, AtomMap};

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct TyFlags: u32 {
        const Any            = 1 << 0;
        const Unknown        = 1 << 1;
        const String         = 1 << 2;
        const Number         = 1 << 3;
        const StringLiteral  = 1 << 7;
        const NumberLiteral  = 1 << 8;
        const BooleanLiteral = 1 << 9;
        const Undefined      = 1 << 15;
        const Null           = 1 << 16;

        const Literal        = Self::StringLiteral.bits() | Self::NumberLiteral.bits() | Self::BooleanLiteral.bits();
        const StringLike     = Self::String.bits() | Self::StringLiteral.bits();
        const NumberLike     = Self::Number.bits() | Self::NumberLiteral.bits();
    }
}

rts_span::new_index!(TyID);

#[derive(Debug, Clone, Copy)]
pub struct Ty<'cx> {
    pub flags: TyFlags,
    pub kind: TyKind<'cx>,
    pub id: TyID,
}

impl<'cx> Ty<'cx> {
    pub fn new(id: TyID, flags: TyFlags, kind: TyKind<'cx>) -> Self {
        Self { flags, kind, id }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TyKind<'cx> {
    NumLit(&'cx NumLitTy),
    Intrinsic(&'cx IntrinsicTy),
}

impl<'cx> TyKind<'cx> {
    pub fn as_str(&self, atoms: &'cx AtomMap) -> &'cx str {
        match self {
            TyKind::NumLit(_) => "number",
            TyKind::Intrinsic(ty) => atoms.get(ty.name),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IntrinsicTy {
    pub name: AtomId,
}

#[derive(Debug, Clone, Copy)]
pub struct NumLitTy {
    pub val: f64,
}
