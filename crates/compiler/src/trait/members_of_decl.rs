pub trait MembersOfDecl {
    fn id(&self) -> bolt_ts_ast::NodeID;
    fn has_static_modifier(&self) -> bool;
}

impl MembersOfDecl for bolt_ts_ast::ObjectTyMember<'_> {
    fn id(&self) -> bolt_ts_ast::NodeID {
        self.id()
    }
    fn has_static_modifier(&self) -> bool {
        use bolt_ts_ast::ObjectTyMemberKind::*;
        let modifiers = match self.kind {
            IndexSig(n) => n.modifiers,
            Prop(n) => n.modifiers,
            Setter(n) => n.modifiers,
            Getter(n) => n.modifiers,
            Method(_) | CallSig(_) | CtorSig(_) => return false,
        };
        modifiers.is_some_and(|mods| mods.flags.contains(bolt_ts_ast::ModifierKind::Static))
    }
}

impl MembersOfDecl for bolt_ts_ast::ClassElem<'_> {
    fn id(&self) -> bolt_ts_ast::NodeID {
        self.id()
    }
    fn has_static_modifier(&self) -> bool {
        use bolt_ts_ast::ClassEleKind::*;
        let modifiers = match self.kind {
            IndexSig(n) => n.modifiers,
            Prop(n) => n.modifiers,
            Setter(n) => n.modifiers,
            Getter(n) => n.modifiers,
            Method(n) => n.modifiers,
            Ctor(_) | StaticBlock(_) => return false,
        };
        modifiers.is_some_and(|mods| mods.flags.contains(bolt_ts_ast::ModifierKind::Static))
    }
}

impl MembersOfDecl for bolt_ts_ast::ObjectMember<'_> {
    fn id(&self) -> bolt_ts_ast::NodeID {
        self.id()
    }
    fn has_static_modifier(&self) -> bool {
        false
    }
}
