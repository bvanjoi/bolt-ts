use rustc_hash::FxHashMap;

use crate::bind::{SymbolFlags, SymbolID, SymbolName};

use super::Resolver;

impl Resolver<'_, '_, '_> {
    pub(super) fn get_exports_of_symbol(
        &mut self,
        symbol: SymbolID,
    ) -> Option<&FxHashMap<SymbolName, SymbolID>> {
        let flags = self.symbol(symbol).flags;
        if flags.intersects(SymbolFlags::MODULE.union(SymbolFlags::CLASS)) {
            self.get_exports_of_module(symbol)
        } else {
            None
        }
    }

    fn get_exports_of_module(
        &mut self,
        symbol: SymbolID,
    ) -> Option<&FxHashMap<SymbolName, SymbolID>> {
        let s = self.symbol(symbol);
        Some(&s.exports.0)
    }
}
