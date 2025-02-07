use rustc_hash::FxHashMap;

use crate::bind::{SymbolFlags, SymbolID, SymbolName};

use super::Resolver;

impl<'cx> Resolver<'cx, '_> {
    pub(super) fn get_exports_of_symbol(
        &mut self,
        symbol: SymbolID,
    ) -> Option<&FxHashMap<SymbolName, SymbolID>> {
        let flags = self.symbol(symbol).flags;
        if flags.intersects(SymbolFlags::CLASS) {
            let c = self.symbol(symbol).expect_class();
            Some(&c.exports)
        } else if flags.intersects(SymbolFlags::MODULE) {
            self.get_exports_of_module(symbol)
        } else {
            None
        }
    }

    fn get_exports_of_module(
        &mut self,
        symbol: SymbolID,
    ) -> Option<&FxHashMap<SymbolName, SymbolID>> {
        // if let Some(exports) = self.resolved_exports.get(&symbol) {
        //     return Some(exports);
        // }
        let s = self.symbol(symbol);
        let ns = s.expect_ns();
        Some(&ns.exports)
    }
}
