use bolt_ts_middle::F64Represent;

#[derive(Debug, Clone, Copy)]
pub struct NumberLitTy<'cx> {
    pub val: F64Represent,
    pub links: super::FreshTyLinksID<'cx>,
}

impl<'cx> NumberLitTy<'cx> {
    pub fn new(val: f64, links: super::FreshTyLinksID<'cx>) -> Self {
        Self {
            val: F64Represent::new(val),
            links,
        }
    }

    pub fn is(&self, val: f64) -> bool {
        self.val.val() == val
    }
}
