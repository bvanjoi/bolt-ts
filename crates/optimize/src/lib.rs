#![allow(dead_code)]

mod emit;

use bolt_ts_checker::check::TyChecker;
use bolt_ts_checker::emit_resolver::EmitResolver;
use bolt_ts_span::ModuleID;

pub use self::emit::Emitter;

pub struct OptimizeAndEmitOutput {
    pub files: Vec<(ModuleID, String)>,
}

pub fn optimize_and_js_emit<'cx>(
    entries: Vec<ModuleID>,
    checker: &mut TyChecker<'cx>,
) -> OptimizeAndEmitOutput {
    let files = entries
        .into_iter()
        .filter_map(|item| {
            let is_default_lib = checker.module_arena.get_module(item).is_default_lib();
            if is_default_lib {
                None
            } else {
                let resolver = EmitResolver::new(checker);
                let origin = resolver.module_content(item).to_string();
                let files_output = emit::emit_js(resolver, item, origin);
                Some((item, files_output))
            }
        })
        .collect::<Vec<_>>();
    OptimizeAndEmitOutput { files }
}
