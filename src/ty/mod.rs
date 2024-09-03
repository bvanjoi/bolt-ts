bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct TyFlags: u32 {
        const Any           = 1 << 0;
        const Unknown       = 1 << 1;
        const String        = 1 << 2;
        const Number        = 1 << 3;
        const NumberLiteral = 1 << 8;

        const Literal = Self::NumberLiteral.bits();
    }
}

crate::new_index!(TyID);

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
}

#[derive(Debug, Clone, Copy)]
pub struct NumLitTy {
    pub val: f64,
}
