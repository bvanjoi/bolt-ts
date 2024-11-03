use super::ast;
use super::TyChecker;
use crate::bind::{SymbolID, SymbolName};

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct SigFlags: u16 {
        const HAS_REST_PARAMETER  = 1 << 0;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Sig<'cx> {
    pub flags: SigFlags,
    pub params: &'cx [SymbolID],
    pub min_args_count: usize,
}

impl Sig<'_> {
    pub fn has_rest_param(&self) -> bool {
        self.flags.contains(SigFlags::HAS_REST_PARAMETER)
    }
}

enum SigDecl<'cx> {
    FnDecl(&'cx ast::FnDecl<'cx>),
}

impl<'cx> SigDecl<'cx> {
    fn params(&self) -> ast::ParamsDecl<'cx> {
        match self {
            SigDecl::FnDecl(f) => f.params,
        }
    }

    fn id(&self) -> ast::NodeID {
        match self {
            SigDecl::FnDecl(f) => f.id,
        }
    }

    fn has_rest_param(&self) -> bool {
        self.params()
            .last()
            .map_or(false, |param| param.dotdotdot.is_some())
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_sig_from_decl(&mut self, node: ast::Node<'cx>) -> Sig<'cx> {
        let id = node.id();
        if !self.node_id_to_sig.contains_key(&id) {
            let sig = get_sig_from_decl(self, node);
            let prev = self.node_id_to_sig.insert(id, sig);
            assert!(prev.is_none());
        }
        self.node_id_to_sig.get(&id).copied().unwrap()
    }
}

fn get_sig_from_decl<'cx>(checker: &TyChecker<'cx>, node: ast::Node<'cx>) -> Sig<'cx> {
    let decl = match node {
        ast::Node::FnDecl(f) => SigDecl::FnDecl(f),
        _ => unreachable!(),
    };
    assert!(!checker.node_id_to_sig.contains_key(&decl.id()));
    let mut flags = SigFlags::empty();
    let mut min_args_count = 0;
    let mut params = Vec::with_capacity(8);
    for (i, param) in decl.params().iter().enumerate() {
        let scope_id = checker.node_id_to_scope_id[&param.id];
        let symbol = checker.res[&(scope_id, SymbolName::Normal(param.name.name))];
        params.push(symbol);
        let is_opt = param.question.is_some() || param.dotdotdot.is_some();
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
    let params: &[SymbolID] = checker.alloc(params);
    Sig {
        flags,
        params,
        min_args_count,
    }
}
