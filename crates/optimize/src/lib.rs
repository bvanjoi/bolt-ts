use crate::lowering::lowering;

mod ir;
mod lowering;

pub fn optimize<'cx>(root: &'cx bolt_ts_ast::Program<'cx>) {
    let ir = lowering(root);
}
