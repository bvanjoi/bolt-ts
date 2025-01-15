use super::TyChecker;
use crate::bind::SymbolID;
use crate::ty::TyID;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ResolutionKey {
    Type(SymbolID),
    ResolvedBaseConstructorType(TyID),
    ResolvedBaseTypes(TyID),
    DeclaredType(SymbolID),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Cycle {
    Some(ResolutionKey),
    None,
}

impl Cycle {
    pub fn has_cycle(&self) -> bool {
        matches!(self, Cycle::Some(_))
    }
}

impl<'cx> TyChecker<'cx> {
    fn resolution_target_has_property(&self, key: ResolutionKey) -> bool {
        match key {
            ResolutionKey::Type(symbol) => self
                .symbol_links
                .get(&symbol)
                .is_some_and(|s| s.get_ty().is_some()),
            ResolutionKey::DeclaredType(symbol) => self
                .symbol_links
                .get(&symbol)
                .is_some_and(|s| s.get_declared_ty().is_some()),
            ResolutionKey::ResolvedBaseConstructorType(ty) => self
                .ty_links
                .get(&ty)
                .is_some_and(|t| t.get_resolved_base_ctor_ty().is_some()),
            ResolutionKey::ResolvedBaseTypes(ty) => self
                .ty_links
                .get(&ty)
                .is_some_and(|t| t.get_resolved_base_tys().is_some()),
        }
    }

    fn find_resolution_cycle_start_index(&self, key: ResolutionKey) -> Option<usize> {
        if self.resolution_tys.is_empty() {
            return None;
        }
        // TODO: resolution_start
        let mut i = (self.resolution_tys.len() - 1) as i32;
        while i >= self.resolution_start {
            let idx = i as usize;
            let t = self.resolution_tys[idx];
            if self.resolution_target_has_property(t) {
                return None;
            } else if t == key {
                return Some(idx);
            } else {
                i -= 1;
            }
        }
        None
    }

    pub(super) fn push_ty_resolution(&mut self, key: ResolutionKey) -> bool {
        if let Some(start) = self.find_resolution_cycle_start_index(key) {
            for index in start..self.resolution_res.len() {
                self.resolution_res[index] = false;
            }
            false
        } else {
            self.resolution_tys.push(key);
            self.resolution_res.push(true);
            true
        }
    }

    /// `Some(xxx)` means there is a cycle, and `xxx` is the start index of the cycle.
    pub(super) fn pop_ty_resolution(&mut self) -> Cycle {
        let key = self.resolution_tys.pop().unwrap();
        (!self.resolution_res.pop().unwrap())
            .then(|| Cycle::Some(key))
            .unwrap_or(Cycle::None)
    }
}
