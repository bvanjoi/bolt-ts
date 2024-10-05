use crate::atoms::{AtomId, AtomMap};

rts_span::new_index!(TyID);

#[derive(Debug, Clone, Copy)]
pub struct Ty<'cx> {
    pub kind: TyKind<'cx>,
    pub id: TyID,
}

impl<'cx> Ty<'cx> {
    pub fn new(id: TyID, kind: TyKind<'cx>) -> Self {
        Self { kind, id }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TyKind<'cx> {
    StringLit,
    NumberLit(&'cx NumberLitTy),
    Array(&'cx ArrayTy<'cx>),
    ArrayLit(&'cx ArrayTy<'cx>),
    Intrinsic(&'cx IntrinsicTy),
    Union(&'cx UnionTy<'cx>),
}

impl<'cx> TyKind<'cx> {
    pub fn to_string(&self, atoms: &'cx AtomMap) -> String {
        match self {
            TyKind::NumberLit(_) => "number".to_string(),
            TyKind::Intrinsic(ty) => atoms.get(ty.name).to_string(),
            TyKind::Array(_) => "array".to_string(),
            TyKind::Union(union) => union
                .tys
                .iter()
                .map(|ty| ty.kind.to_string(atoms))
                .collect::<Vec<_>>()
                .join(" | "),
            TyKind::StringLit => todo!(),
            TyKind::ArrayLit(_) => todo!(),
        }
    }

    pub fn is_union(&self) -> bool {
        matches!(self, TyKind::Union(_))
    }

    pub fn is_number(&self) -> bool {
        use TyKind::*;
        if let Intrinsic(ty) = self {
            matches!(ty.kind, IntrinsicTyKind::Number)
        } else {
            false
        }
    }

    pub fn is_union_or_intersection(&self) -> bool {
        self.is_union()
    }

    pub fn is_lit(&self) -> bool {
        use TyKind::*;
        if matches!(self, StringLit | NumberLit(_)) {
            true
        } else if let Intrinsic(ty) = self {
            ty.kind.is_lit()
        } else {
            false
        }
    }

    pub fn is_number_like(&self) -> bool {
        use TyKind::*;
        if matches!(self, NumberLit(_)) {
            true
        } else if let Intrinsic(ty) = self {
            ty.kind.is_number_like()
        } else {
            false
        }
    }

    pub fn is_string_like(&self) -> bool {
        use TyKind::*;
        if matches!(self, StringLit) {
            true
        } else if let Intrinsic(ty) = self {
            ty.kind.is_string_like()
        } else {
            false
        }
    }

    pub fn is_boolean_like(&self) -> bool {
        use TyKind::*;
        if let Intrinsic(ty) = self {
            ty.kind.is_boolean_like()
        } else {
            false
        }
    }

    pub fn is_structured(&self) -> bool {
        self.is_union()
    }

    pub fn is_structured_or_instantiable(&self) -> bool {
        self.is_structured()
    }

    pub fn definitely_non_nullable(&self) -> bool {
        self.is_number_like() || self.is_string_like()
    }

    pub fn is_nullable(&self) -> bool {
        use TyKind::*;
        if let Intrinsic(ty) = self {
            ty.kind.is_nullable()
        } else {
            false
        }
    }

    pub fn is_fresh(&self) -> bool {
        self.is_lit()
    }
}

pub type Tys<'cx> = &'cx [&'cx Ty<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct UnionTy<'cx> {
    pub tys: Tys<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ArrayTy<'cx> {
    pub ty: &'cx Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct IntrinsicTy {
    pub name: AtomId,
    pub kind: IntrinsicTyKind,
}

#[derive(Debug, Clone, Copy)]
pub enum IntrinsicTyKind {
    Any,
    Unknown,
    Null,
    Undefined,
    String,
    Number,
    True,
    False,
    Error,
}

impl IntrinsicTyKind {
    fn is_lit(self) -> bool {
        use IntrinsicTyKind::*;
        matches!(self, True | False)
    }

    fn is_number_like(&self) -> bool {
        use IntrinsicTyKind::*;
        matches!(self, Number)
    }

    fn is_string_like(&self) -> bool {
        use IntrinsicTyKind::*;
        matches!(self, String)
    }

    fn is_boolean_like(&self) -> bool {
        use IntrinsicTyKind::*;
        matches!(self, True | False)
    }

    fn is_nullable(&self) -> bool {
        use IntrinsicTyKind::*;
        matches!(self, Null | Undefined)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NumberLitTy {
    pub val: f64,
}
