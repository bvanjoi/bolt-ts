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

impl<'cx> DeclarationEmitter<'cx> {
    fn emit_list<T>(
        &mut self,
        list: &[T],
        emit_item: impl Fn(&mut Self, &T),
        emit_sep: impl Fn(&mut Self, &T),
    ) {
        for (idx, item) in list.iter().enumerate() {
            emit_item(self, item);
            if idx != list.len() - 1 {
                emit_sep(self, item)
            }
        }
    }
}

impl<'cx> ast::Visitor<'cx> for DeclarationEmitter<'cx> {
    fn visit_interface_decl(&mut self, node: &'cx ast::InterfaceDecl<'cx>) {
        self.emitter.print().p("interface");
        self.emitter.print().p_whitespace();
        self.emitter.emit_atom(node.name.name);
        self.emitter.print().p_whitespace();
        if let Some(extends) = node.extends {
            self.emitter.print().p("extends");
            self.emitter.print().p_whitespace();
            for item in extends.list {
                self.visit_refer_ty(item);
            }
        }
        self.emitter.print().p_l_brace();
        if !node.members.is_empty() {
            self.emitter.increment_indent();
            self.emitter.print().p_newline();
            self.emit_list(
                node.members,
                |this, elem| {
                    use ast::ObjectTyMemberKind::*;
                    match elem.kind {
                        IndexSig(n) => todo!(),
                        Prop(n) => this.visit_prop_signature(n),
                        Method(n) => todo!(),
                        CallSig(n) => todo!(),
                        CtorSig(n) => todo!(),
                        Setter(n) => todo!(),
                        Getter(n) => todo!(),
                    }
                },
                |this, _| {
                    this.emitter.print().p_newline();
                },
            );
            self.emitter.decrement_indent();
            self.emitter.print().p_newline();
        }
        self.emitter.print().p_r_brace();
    }

    // TODO: merge this and `JsEmitter::visit_prop_name`
    fn visit_prop_name(&mut self, node: &'cx bolt_ts_ast::PropName<'cx>) {
        use bolt_ts_ast::PropNameKind::*;
        match &node.kind {
            Ident(n) => self.visit_ident(n),
            StringLit { .. } => todo!(),
            NumLit(n) => todo!(),
            Computed(n) => {
                todo!()
            }
        }
    }

    fn visit_prop_signature(&mut self, node: &'cx ast::PropSignature<'cx>) {
        self.visit_prop_name(node.name);
        if node.question.is_some() {
            self.emitter.print().p_question();
        }
        if let Some(ty) = node.ty {
            self.emitter.print().p_colon();
            self.emitter.print().p_whitespace();
            self.visit_ty(ty);
        }
        self.emitter.print().p_semi();
    }

    fn visit_refer_ty(&mut self, n: &'cx ast::ReferTy<'cx>) {
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

    fn visit_ident(&mut self, node: &'cx ast::Ident) {
        self.emitter.emit_atom(node.name);
    }
}
