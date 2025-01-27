use super::{Ty, Tys};

#[derive(Clone, Copy, Debug)]
pub enum TyMapper<'cx> {
    Simple(SimpleTyMapper<'cx>),
    Array(ArrayTyMapper<'cx>),
    Fn(FnTyMapper<'cx>),
    Composite(CompositeTyMapper<'cx>),
    Merged(MergedTyMapper<'cx>),
}

impl<'cx> TyMapper<'cx> {
    pub fn create(sources: Tys<'cx>, targets: Tys<'cx>) -> Self {
        if sources.len() == 1 {
            let target = if !targets.is_empty() {
                targets[0]
            } else {
                todo!("use any")
            };
            let mapper = SimpleTyMapper {
                source: sources[0],
                target,
            };
            TyMapper::Simple(mapper)
        } else {
            let mapper = ArrayTyMapper {
                sources,
                targets: Some(targets),
            };
            TyMapper::Array(mapper)
        }
    }

    pub fn make_composite(m1: &'cx TyMapper<'cx>, m2: &'cx TyMapper<'cx>) -> TyMapper<'cx> {
        TyMapper::Composite(CompositeTyMapper {
            mapper1: m1,
            mapper2: m2,
        })
    }

    pub fn make_unary(source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> TyMapper<'cx> {
        TyMapper::Simple(SimpleTyMapper { source, target })
    }
}

macro_rules! ty_mapper {
    ($kind: ident, $ty: ty, $as_kind: ident, $is_kind: ident) => {
        impl<'cx> TyMapper<'cx> {
            #[inline(always)]
            pub fn $as_kind(&self) -> Option<$ty> {
                match self {
                    TyMapper::$kind(ty) => Some(ty),
                    _ => None,
                }
            }
            #[inline(always)]
            pub fn $is_kind(&self) -> bool {
                self.$as_kind().is_some()
            }
        }
    };
}

ty_mapper!(Simple, &SimpleTyMapper<'cx>, as_simple, is_simple);
ty_mapper!(Array, &ArrayTyMapper<'cx>, as_array, is_array);
ty_mapper!(Fn, &FnTyMapper<'cx>, as_fn, is_fn);
ty_mapper!(
    Composite,
    &CompositeTyMapper<'cx>,
    as_composite,
    is_composite
);
ty_mapper!(Merged, &MergedTyMapper<'cx>, as_merged, is_merged);

#[derive(Clone, Copy, Debug)]
pub struct SimpleTyMapper<'cx> {
    pub source: &'cx Ty<'cx>,
    pub target: &'cx Ty<'cx>,
}

#[derive(Clone, Copy, Debug)]
pub struct ArrayTyMapper<'cx> {
    pub sources: Tys<'cx>,
    pub targets: Option<Tys<'cx>>,
}

#[derive(Clone, Copy, Debug)]
pub struct FnTyMapper<'cx> {
    func: [Ty<'cx>; 2],
}

#[derive(Clone, Copy, Debug)]
pub struct CompositeTyMapper<'cx> {
    pub mapper1: &'cx TyMapper<'cx>,
    pub mapper2: &'cx TyMapper<'cx>,
}

#[derive(Clone, Copy, Debug)]
pub struct MergedTyMapper<'cx> {
    pub mapper1: &'cx TyMapper<'cx>,
    pub mapper2: &'cx TyMapper<'cx>,
}
