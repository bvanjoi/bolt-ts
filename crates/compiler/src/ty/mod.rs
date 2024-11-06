mod facts;

use crate::atoms::{AtomId, AtomMap};
use crate::bind::SymbolID;
pub use facts::{has_type_facts, TypeFacts};
use rustc_hash::FxHashMap;

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
    Intrinsic(&'cx IntrinsicTy),
    Union(&'cx UnionTy<'cx>),
    Object(&'cx ObjectTy<'cx>),
}

impl<'cx> TyKind<'cx> {
    pub fn to_string(&self, atoms: &'cx AtomMap) -> String {
        match self {
            TyKind::NumberLit(_) => "number".to_string(),
            TyKind::Intrinsic(ty) => atoms.get(ty.name).to_string(),
            TyKind::Union(union) => union
                .tys
                .iter()
                .map(|ty| ty.kind.to_string(atoms))
                .collect::<Vec<_>>()
                .join(" | "),
            TyKind::StringLit => todo!(),

            TyKind::Object(object) => object.kind.as_str().to_string(),
        }
    }

    pub fn is_union(&self) -> bool {
        matches!(self, TyKind::Union(_))
    }

    pub fn is_any(&self) -> bool {
        use TyKind::*;
        if let Intrinsic(ty) = self {
            matches!(ty.kind, IntrinsicTyKind::Any)
        } else {
            false
        }
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

    pub fn is_object_or_intersection(&self) -> bool {
        self.is_object()
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
        self.is_union() | self.is_object()
    }

    pub fn is_object(&self) -> bool {
        matches!(self, TyKind::Object(_))
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

    pub fn as_fn(&self) -> Option<&'cx FnTy<'cx>> {
        use TyKind::*;
        if let Object(ty) = self {
            if let ObjectTyKind::Fn(f) = ty.kind {
                Some(f)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn as_object_lit(&self) -> Option<&'cx ObjectLitTy<'cx>> {
        use TyKind::*;
        if let Object(ty) = self {
            if let ObjectTyKind::Lit(object) = ty.kind {
                Some(object)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn as_array(&self) -> Option<&'cx ArrayTy<'cx>> {
        use TyKind::*;
        if let Object(ty) = self {
            if let ObjectTyKind::Array(array) = ty.kind {
                Some(array)
            } else {
                None
            }
        } else {
            None
        }
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
    Void,
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

#[derive(Debug, Clone, Copy)]
pub struct ObjectTy<'cx> {
    pub kind: ObjectTyKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ObjectTyKind<'cx> {
    Class(&'cx ClassTy),
    Fn(&'cx FnTy<'cx>),
    Lit(&'cx ObjectLitTy<'cx>),
    Array(&'cx ArrayTy<'cx>),
}

impl ObjectTyKind<'_> {
    fn as_str(&self) -> &'static str {
        match self {
            ObjectTyKind::Class(_) => "class",
            ObjectTyKind::Fn(_) => "function",
            ObjectTyKind::Lit(_) => "Object",
            ObjectTyKind::Array(_) => "array",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ClassTy {}

#[derive(Debug, Clone, Copy)]
pub struct ObjectLitTy<'cx> {
    pub members: &'cx FxHashMap<AtomId, &'cx Ty<'cx>>,
    pub symbol: SymbolID,
}

#[derive(Debug, Clone, Copy)]
pub struct FnTy<'cx> {
    pub params: &'cx [&'cx Ty<'cx>],
    pub ret: &'cx Ty<'cx>,
}
