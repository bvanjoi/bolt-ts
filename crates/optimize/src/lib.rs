mod emit;
mod interpreter;
mod ir;
mod lowering;
mod pipeline;

use bolt_ts_checker::check::TyChecker;
use bolt_ts_span::ModuleID;

pub use self::emit::Emitter;
pub use self::lowering::LoweringResult;
use self::lowering::lowering;

pub struct IrOutput {
    pub lowered: LoweringResult,
}

pub struct OptimizeAndEmitOutput {
    pub files: Vec<(ModuleID, String)>,
    pub ir: Vec<(ModuleID, IrOutput)>,
}

pub fn optimize_and_js_emit<'cx>(
    entries: Vec<ModuleID>,
    mut checker: TyChecker<'cx>,
) -> OptimizeAndEmitOutput {
    let output = entries
        .into_iter()
        .filter_map(|item| {
            let is_default_lib = checker.module_arena.get_module(item).is_default_lib();
            if is_default_lib {
                None
            } else {
                let mut ir = lowering(item, &mut checker);
                pipeline::reducer::ReduceGraph::new(&mut ir.nodes, &mut ir.graph_arena);
                let files_output = emit::emit_js(&checker.atoms, &ir);
                let ir_output = IrOutput { lowered: ir };
                Some((item, (files_output, ir_output)))
            }
        })
        .collect::<Vec<_>>();
    let output = output
        .into_iter()
        .map(|(m, (file, ir))| ((m, file), (m, ir)))
        .collect::<(Vec<_>, Vec<_>)>();
    OptimizeAndEmitOutput {
        files: output.0,
        ir: output.1,
    }
}
