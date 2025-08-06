mod emit;
mod interpreter;
mod ir;
mod lowering;
mod pipeline;

use bolt_ts_checker::check::TyChecker;
use bolt_ts_span::ModuleID;

use crate::lowering::lowering;

pub fn optimize_and_emit<'cx>(
    entries: Vec<ModuleID>,
    mut checker: TyChecker<'cx>,
) -> Vec<(ModuleID, String)> {
    entries
        .into_iter()
        .filter_map(|item| {
            let is_default_lib = checker.module_arena.get_module(item).is_default_lib();
            if is_default_lib {
                None
            } else {
                let mut ir = lowering(item, &mut checker);
                pipeline::reducer::ReduceGraph::new(&mut ir.nodes, &mut ir.graph_arena);
                let output = emit::emit(&checker.atoms, &ir);
                Some((item, output))
            }
        })
        .collect()
}
