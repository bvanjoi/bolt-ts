mod ast;
mod atoms;
mod bind;
pub mod check;
mod emit;
mod errors;
mod keyword;
pub mod parser;
mod ty;

use std::borrow::Cow;
use std::path::PathBuf;

use atoms::AtomMap;
use bind::Binder;
use bolt_ts_span::{ModuleArena, ModuleID, ModulePath};
use parser::token::TokenKind;
use rustc_hash::FxHashMap;

type Diag = Box<dyn bolt_ts_errors::miette::Diagnostic + Send + Sync + 'static>;

pub struct Output {
    pub module_arena: ModuleArena,
    pub output: String,
    pub diags: Vec<bolt_ts_errors::Diag>,
}

fn current_exe_dir() -> std::path::PathBuf {
    std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

pub fn eval_from(m: ModulePath) -> Output {
    let mut module_arena = ModuleArena::new();
    let m = module_arena.new_module(m);
    let mut atoms = AtomMap::default();
    // atom init
    if cfg!(debug_assertions) {
        for (idx, (name, _)) in keyword::KEYWORDS.iter().enumerate() {
            let t = unsafe { std::mem::transmute::<u8, TokenKind>(idx as u8) };
            assert!(t.is_keyword());
            assert_eq!(format!("{t:?}").to_lowercase(), *name);
        }
    }
    for (atom, id) in keyword::KEYWORDS {
        atoms.insert(*id, Cow::Borrowed(atom));
    }
    for (atom, id) in keyword::IDENTIFIER {
        atoms.insert(*id, Cow::Borrowed(atom))
    }

    // parser
    let mut p = parser::Parser::new();
    let parse_arena = bumpalo::Bump::new();
    let result = parser::parse(&mut atoms, &parse_arena, &module_arena, m.id);
    p.insert(m.id, result);

    let dir = current_exe_dir();
    for (_, file) in bolt_ts_lib::LIB_ENTIRES {
        let m = module_arena.new_module(ModulePath::Real(dir.join(file)));
        let result = parser::parse(&mut atoms, &parse_arena, &module_arena, m.id);
        assert!(result.diags.is_empty());
        p.insert(m.id, result)
    }

    // bind
    let mut binder = bind::Binder::new(&atoms);
    binder.bind_program(p.get(m.id).root);
    let Binder {
        scope_id_parent_map,
        node_id_to_scope_id,
        symbols,
        res,
        final_res,
        ..
    } = binder;

    // type check
    let ty_arena = bumpalo::Bump::new();
    let mut c = check::TyChecker::new(
        &ty_arena,
        &p.get(m.id).nodes(),
        &p.get(m.id).parent_map(),
        &atoms,
        &scope_id_parent_map,
        &node_id_to_scope_id,
        &symbols,
        &res,
        final_res,
    );
    c.check_program(p.get(m.id).root);

    // emit
    let mut emitter = emit::Emit::new(&c.atoms);
    let output = emitter.emit(p.get(m.id).root);

    let diags: Vec<_> = c.diags.into_iter().chain(p.steal_errors(m.id)).collect();
    Output {
        module_arena,
        diags,
        output,
    }
}

pub fn eval_and_emit(entry: PathBuf) {
    let output = eval_from(ModulePath::Real(entry));
    output
        .diags
        .into_iter()
        .for_each(|diag| diag.emit(&output.module_arena));
}

const RED_ZONE: usize = 100 * 1024; // 100k
const STACK_PER_RECURSION: usize = 1 * 1024 * 1024; // 1MB

fn ensure_sufficient_stack<R, F: FnOnce() -> R>(f: F) -> R {
    stacker::maybe_grow(RED_ZONE, STACK_PER_RECURSION, f)
}
