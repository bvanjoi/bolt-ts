mod node_links;
mod sig_links;
mod symbol_links;
mod ty_links;

macro_rules! _links {
    ($s: ident, $(($x: ident, $ty: ty)),* $( , )? ) => {
        #[derive(Debug, Default, Clone, Copy)]
        pub struct $s<'cx> {
            $(
                $x: Option<$ty>,
            )*
        }

        impl<'cx> $s<'cx> {
            paste::paste! {
                $(
                    #[allow(unused)]
                    #[track_caller]
                    pub fn [<with_ $x>](mut self, $x: $ty) -> Self {
                        self.[<set_ $x>]($x);
                        self
                    }
                    #[allow(unused)]
                    #[track_caller]
                    pub fn [<set_ $x>](&mut self, $x: $ty) {
                        assert!(self.$x.is_none());
                        self.$x = Some($x);
                    }
                    #[allow(unused)]
                    #[track_caller]
                    pub fn [<override_ $x>](&mut self, $x: $ty) {
                        assert!(self.$x.is_some());
                        self.$x = Some($x);
                    }
                    #[allow(unused)]
                    #[track_caller]
                    pub fn [<get_ $x>](&self) -> Option<$ty> {
                        self.$x
                    }
                    #[allow(unused)]
                    #[track_caller]
                    pub fn [<expect_ $x>](&self) -> $ty {
                        self.$x.unwrap()
                    }
                    #[allow(unused)]
                    #[track_caller]
                    pub fn [<config_ $x>](&mut self, f: impl FnOnce($ty) -> $ty) {
                        self.$x = match self.$x {
                            Some(c) => Some(f(c)),
                            None => unreachable!("`{}` is not defined", stringify!($x)),
                        };
                    }
                )*
            }
        }
    };
}

use _links as links;
use bolt_ts_ast::NodeID;

pub use node_links::NodeLinks;
pub use sig_links::SigLinks;
pub use symbol_links::SymbolLinks;
pub use ty_links::TyLinks;

use super::NodeCheckFlags;
use super::ty::{SigID, TyID};

impl<'cx> super::TyChecker<'cx> {
    pub fn get_node_links(&mut self, node: NodeID) -> &NodeLinks<'cx> {
        self.node_links
            .entry(node)
            .or_insert_with(|| NodeLinks::default().with_flags(NodeCheckFlags::empty()))
    }

    #[track_caller]
    pub fn get_mut_node_links(&mut self, node: NodeID) -> &mut NodeLinks<'cx> {
        self.node_links.get_mut(&node).unwrap()
    }

    pub fn get_sig_links(&mut self, sig: SigID) -> &SigLinks<'cx> {
        self.sig_links.entry(sig).or_default()
    }

    pub fn get_mut_sig_links(&mut self, sig: SigID) -> &mut SigLinks<'cx> {
        self.sig_links.get_mut(&sig).unwrap()
    }

    pub fn get_ty_links(&mut self, ty: TyID) -> &TyLinks<'cx> {
        self.ty_links.entry(ty).or_default()
    }

    pub fn get_mut_ty_links(&mut self, ty: TyID) -> &mut TyLinks<'cx> {
        self.ty_links.get_mut(&ty).unwrap()
    }

    pub fn expect_ty_links(&self, ty: TyID) -> &TyLinks<'cx> {
        &self.ty_links[&ty]
    }
}
