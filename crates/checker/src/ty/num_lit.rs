use bolt_ts_binder::SymbolID;
use bolt_ts_middle::F64Represent;

#[derive(Debug, Clone)]
pub struct NumberLitTy<'cx> {
    pub val: F64Represent,
    pub links: super::FreshTyLinksID<'cx>,
    pub symbol: Option<SymbolID>,
}

impl<'cx> NumberLitTy<'cx> {
    pub fn new(val: f64, symbol: Option<SymbolID>, links: super::FreshTyLinksID<'cx>) -> Self {
        Self {
            val: F64Represent::new(val),
            links,
            symbol,
        }
    }

    pub fn is(&self, val: f64) -> bool {
        self.val.val() == val
    }
}
