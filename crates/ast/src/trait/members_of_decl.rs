pub trait MembersOfDecl {
    fn id(&self) -> crate::NodeID;
    fn has_static_modifier(&self) -> bool;
}

impl MembersOfDecl for crate::ObjectTyMember<'_> {
    fn id(&self) -> crate::NodeID {
        self.id()
    }
    fn has_static_modifier(&self) -> bool {
        use crate::ObjectTyMemberKind::*;
        let modifiers = match self.kind {
            IndexSig(n) => n.modifiers,
            Prop(n) => n.modifiers,
            Setter(n) => n.modifiers,
            Getter(n) => n.modifiers,
            Method(_) | CallSig(_) | CtorSig(_) => return false,
        };
        modifiers.is_some_and(|mods| mods.flags.contains(crate::ModifierKind::Static))
    }
}

impl MembersOfDecl for crate::ClassElem<'_> {
    fn id(&self) -> crate::NodeID {
        self.id()
    }
    fn has_static_modifier(&self) -> bool {
        use crate::ClassElemKind::*;
        let modifiers = match self.kind {
            IndexSig(n) => n.modifiers,
            Prop(n) => n.modifiers,
            Setter(n) => n.modifiers,
            Getter(n) => n.modifiers,
            Method(n) => n.modifiers,
            Ctor(_) | StaticBlock(_) => return false,
        };
        modifiers.is_some_and(|mods| mods.flags.contains(crate::ModifierKind::Static))
    }
}

impl MembersOfDecl for crate::ObjectMember<'_> {
    fn id(&self) -> crate::NodeID {
        self.id()
    }
    fn has_static_modifier(&self) -> bool {
        false
    }
}
