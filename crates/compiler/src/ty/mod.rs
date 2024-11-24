mod facts;

use crate::atoms::{AtomId, AtomMap};
use crate::bind::SymbolID;
use crate::keyword;
pub use facts::{has_type_facts, TypeFacts};
use rustc_hash::FxHashMap;

rts_span::new_index!(TyID);
rts_span::new_index!(TyVarID);

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
    Var(TyVarID),
}

impl<'cx> TyKind<'cx> {
    pub fn to_string(&self, atoms: &'cx AtomMap) -> String {
        match self {
            TyKind::NumberLit(_) => "number".to_string(),
            TyKind::Intrinsic(ty) => ty.kind.as_str().to_string(),
            TyKind::Union(union) => union
                .tys
                .iter()
                .map(|ty| ty.kind.to_string(atoms))
                .collect::<Vec<_>>()
                .join(" | "),
            TyKind::StringLit => todo!(),
            TyKind::Object(object) => object.kind.to_string(atoms),
            TyKind::Var(id) => {
                // todo: delay bug
                format!("#{id:#?}")
            }
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

    pub fn is_ty_var(&self) -> bool {
        matches!(self, TyKind::Var(_))
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

    pub fn as_class(&self) -> Option<&'cx ClassTy> {
        use TyKind::*;
        if let Object(ty) = self {
            if let ObjectTyKind::Class(class) = ty.kind {
                Some(class)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn as_interface(&self) -> Option<&'cx InterfaceTy> {
        use TyKind::*;
        if let Object(ty) = self {
            if let ObjectTyKind::Interface(interface) = ty.kind {
                Some(interface)
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

impl<'cx> IntrinsicTyKind {
    fn as_str(&self) -> &'static str {
        match self {
            IntrinsicTyKind::Any => keyword::IDENT_ANY_STR,
            IntrinsicTyKind::Unknown => todo!(),
            IntrinsicTyKind::Void => keyword::IDENT_VOID_STR,
            IntrinsicTyKind::Null => keyword::KW_NULL_STR,
            IntrinsicTyKind::Undefined => keyword::IDENT_UNDEFINED_STR,
            IntrinsicTyKind::String => keyword::IDENT_STRING_STR,
            IntrinsicTyKind::Number => keyword::IDENT_NUMBER_STR,
            IntrinsicTyKind::True => keyword::KW_TRUE_STR,
            IntrinsicTyKind::False => keyword::KW_FALSE_STR,
            IntrinsicTyKind::Error => keyword::IDENT_ERROR_STR,
        }
    }
}

impl std::fmt::Display for IntrinsicTyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
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
    Interface(&'cx InterfaceTy<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct IndexInfo<'cx> {
    pub key_ty: &'cx Ty<'cx>,
    pub val_ty: &'cx Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct InterfaceTy<'cx> {
    pub symbol: SymbolID,
    pub declared_props: &'cx [SymbolID],
    pub base_tys: &'cx [&'cx Ty<'cx>],
    pub index_infos: &'cx [&'cx IndexInfo<'cx>],
}

impl<'cx> ObjectTyKind<'cx> {
    fn to_string(&self, atoms: &AtomMap<'cx>) -> String {
        match self {
            ObjectTyKind::Class(_) => "class".to_string(),
            ObjectTyKind::Fn(_) => "function".to_string(),
            ObjectTyKind::Lit(_) => "Object".to_string(),
            ObjectTyKind::Array(ArrayTy { ty }) => format!("{}[]", ty.kind.to_string(atoms)),
            ObjectTyKind::Interface(_) => "interface".to_string(),
        }
    }

    pub fn is_reference(&self) -> bool {
        matches!(self, ObjectTyKind::Interface(_))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ClassTy {
    pub symbol: SymbolID,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectLitTy<'cx> {
    pub members: &'cx FxHashMap<AtomId, &'cx Ty<'cx>>,
    pub symbol: SymbolID,
}

#[derive(Debug, Clone, Copy)]
pub struct FnTy<'cx> {
    pub params: &'cx [&'cx Ty<'cx>],
    pub ret: &'cx Ty<'cx>,
    pub symbol: SymbolID,
}
