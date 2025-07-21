mod emit;
mod ir;
mod lowering;

use crate::lowering::lowering;

pub fn optimize_and_emit<'cx>(
    atoms: &bolt_ts_atom::AtomMap<'cx>,
    root: &'cx bolt_ts_ast::Program<'cx>,
) -> String {
    let ir = lowering(root);
    emit::emit(atoms, &ir)
}
