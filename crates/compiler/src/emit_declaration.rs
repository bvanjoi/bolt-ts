use bolt_ts_ast::{self as ast, Visitor};
use bolt_ts_atom::AtomIntern;
use bolt_ts_checker::check::TyChecker;
use bolt_ts_optimize::Emitter;
use bolt_ts_span::ModuleID;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

pub fn emit_declaration_parallel(
    entries: &Vec<ModuleID>,
    checker: &TyChecker,
) -> Vec<(ModuleID, String)> {
    let p = checker.p;
    let atoms = &checker.atoms;
    let module_arena = checker.module_arena;
    let output = entries
        .into_par_iter()
        .filter_map(|&item| {
            let is_default_lib = module_arena.get_module(item).is_default_lib();
            if is_default_lib {
                None
            } else {
                let root = p.root(item);
                let output = emit_declaration(&atoms, root);
                Some((item, output))
            }
        })
        .collect::<Vec<_>>();
    output
}

pub fn emit_declaration(atoms: &AtomIntern, root: &ast::Program<'_>) -> String {
    let emitter = Emitter::new(atoms);
    let mut emitter = DeclarationEmitter { emitter };
    emitter.visit_program(root);
    emitter.emitter.print().take_content()
}

struct DeclarationEmitter<'cx> {
    emitter: Emitter<'cx>,
}

impl<'cx> ast::Visitor<'cx> for DeclarationEmitter<'cx> {
    fn visit_interface_decl(&mut self, node: &'cx ast::InterfaceDecl<'cx>) {
        self.emitter.print().p("interface");
        self.emitter.print().p_whitespace();
        self.emitter.emit_atom(node.name.name);
        self.emitter.print().p_whitespace();
        self.emitter.print().p("extends");
        self.emitter.print().p_whitespace();
        if let Some(extends) = node.extends {
            for item in extends.list {
                self.visit_refer_ty(item);
            }
        }
        self.emitter.print().p_l_brace();
        self.emitter.print().p_r_brace();
    }

    fn visit_refer_ty(&mut self, n: &'cx bolt_ts_ast::ReferTy<'cx>) {
        self.visit_entity_name(n.name);
        if let Some(ty_args) = n.ty_args {
            if !ty_args.list.is_empty() {
                self.emitter.print().p_less();
                for ty in ty_args.list {
                    self.visit_ty(ty);
                }
                self.emitter.print().p_great();
            }
        }
    }

    fn visit_ident(&mut self, node: &'cx bolt_ts_ast::Ident) {
        self.emitter.emit_atom(node.name);
    }
}
