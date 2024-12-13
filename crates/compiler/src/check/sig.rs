use super::ast;
use super::TyChecker;
use crate::bind::SymbolID;
use crate::ty::{Sig, SigDecl, SigFlags};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_sig_from_decl(&mut self, id: ast::NodeID) -> &'cx Sig<'cx> {
        if let Some(sig) = self.node_id_to_sig.get(&id) {
            return sig;
        }
        let node = self.p.node(id);
        let sig = get_sig_from_decl(self, node);
        let sig = self.alloc(sig);
        let prev = self.node_id_to_sig.insert(id, sig);
        assert!(prev.is_none());
        sig
    }

    pub(super) fn get_sigs_of_symbol(&mut self, id: SymbolID) -> &'cx [&'cx Sig<'cx>] {
        let f = self.binder.symbol(id).expect_fn();
        let sigs = f
            .decls
            .clone()
            .into_iter()
            .map(|id| self.get_sig_from_decl(id))
            .collect::<Vec<_>>();
        self.alloc(sigs)
    }
}

fn get_sig_from_decl<'cx>(checker: &mut TyChecker<'cx>, node: ast::Node<'cx>) -> Sig<'cx> {
    let decl = match node {
        ast::Node::FnDecl(f) => SigDecl::FnDecl(f),
        ast::Node::FnExpr(f) => SigDecl::FnExpr(f),
        ast::Node::ArrowFnExpr(f) => SigDecl::ArrowFnExpr(f),
        ast::Node::ClassDecl(c) => SigDecl::ClassDecl(c),
        ast::Node::ClassCtor(c) => SigDecl::ClassCtor(c),
        ast::Node::CtorSigDecl(c) => SigDecl::CtorSigDecl(c),
        _ => unreachable!("{node:#?}"),
    };
    assert!(!checker.node_id_to_sig.contains_key(&decl.id()));
    let mut flags = SigFlags::empty();
    let mut min_args_count = 0;
    let mut params = Vec::with_capacity(8);
    for (i, param) in decl.params().iter().enumerate() {
        let symbol = checker.binder.final_res(param.id);
        params.push(symbol);
        let is_opt = param.question.is_some() || param.dotdotdot.is_some() || param.init.is_some();
        if !is_opt {
            min_args_count = params.len();
        } else {
            assert!(
                min_args_count < params.len(),
                "required parameters cannot follow an optional parameter."
            )
        }
    }
    if decl.has_rest_param() {
        flags.insert(SigFlags::HAS_REST_PARAMETER);
    }
    if let SigDecl::ClassDecl(c) = decl {
        if let Some(mods) = c.modifiers {
            if mods
                .list
                .iter()
                .any(|m| m.kind == ast::ModifierKind::Abstract)
            {
                flags.insert(SigFlags::HAS_ABSTRACT);
            }
        }
    }
    let params: &[SymbolID] = checker.alloc(params);
    let ret = match node {
        ast::Node::FnDecl(decl) => None,
        ast::Node::FnExpr(_) => None,
        ast::Node::ArrowFnExpr(_) => None,
        ast::Node::ClassDecl(_) => None,
        ast::Node::ClassCtor(c) => {
            let class_id = checker.p.parent(c.id).unwrap();
            Some(checker.get_symbol_of_decl(class_id))
        }
        ast::Node::CtorSigDecl(c) => {
            if let Some(ret) = c.ty {
                // Some(checker.binder.final_res(ret.id()))
                None
            } else {
                None
            }
        }
        _ => unreachable!(),
    };
    Sig {
        flags,
        params,
        min_args_count,
        ret,
    }
}
