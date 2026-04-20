use super::TyChecker;
use super::ty;
use super::ty::ElementFlags;
use super::ty::SigKind;

use bolt_ts_ast as ast;
use bolt_ts_binder::Symbol;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_ty::TypeFlags;

impl<'cx> TyChecker<'cx> {
    pub fn print_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        enclosing_declaration: Option<ast::NodeID>,
    ) -> &str {
        if self.type_name.contains_key(&ty.id) {
            return &self.type_name[&ty.id];
        }
        self.type_name
            .insert(ty.id, std::borrow::Cow::Borrowed("..."));
        let mut ctx = Ctx {
            c: self,
            enclosing_declaration,
        };
        let type_name = ctx.print_ty(ty);
        let prev = self
            .type_name
            .insert(ty.id, std::borrow::Cow::Owned(type_name));
        debug_assert!(prev.is_some_and(|s| s == "..."));
        &self.type_name[&ty.id]
    }

    fn symbol_value_declaration_is_context_sensitive(
        &self,
        symbol: bolt_ts_binder::SymbolID,
    ) -> bool {
        let s = self.symbol(symbol);
        let Some(id) = s.value_decl else {
            return false;
        };
        self.p.node(id).is_expression() && self.is_context_sensitive(id)
    }

    pub fn get_ty_names_for_error_display(
        &mut self,
        left: &'cx ty::Ty<'cx>,
        right: &'cx ty::Ty<'cx>,
    ) -> (String, String) {
        let left = if left
            .symbol()
            .is_some_and(|symbol| self.symbol_value_declaration_is_context_sensitive(symbol))
        {
            let s = left.symbol().unwrap();
            let decl = self.symbol(s).value_decl;
            debug_assert!(decl.is_some());
            self.print_ty(left, decl)
        } else {
            self.print_ty(left, None)
        }
        .to_string();
        let right = if right
            .symbol()
            .is_some_and(|symbol| self.symbol_value_declaration_is_context_sensitive(symbol))
        {
            let s = right.symbol().unwrap();
            let decl = self.symbol(s).value_decl;
            debug_assert!(decl.is_some());
            self.print_ty(right, decl)
        } else {
            self.print_ty(right, None)
        }
        .to_string();
        // TODO: left_str == right_str
        (left, right)
    }
}

struct Ctx<'a, 'cx> {
    c: &'a mut TyChecker<'cx>,
    enclosing_declaration: Option<ast::NodeID>,
}

impl<'a, 'cx> Ctx<'a, 'cx> {
    fn print_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> String {
        if let Some(alias_symbol) = ty.alias_symbol() {
            let s = self.c.binder.symbol(alias_symbol);
            return s.name.to_string(&self.c.atoms);
        } else if ty.kind.is_array(self.c) {
            let ele = self.c.get_ty_arguments(ty)[0];
            let ele = self.c.print_ty(ele, self.enclosing_declaration);
            return format!("{ele}[]");
        } else if ty == self.c.boolean_ty() {
            return "boolean".to_string();
        }

        match ty.kind {
            ty::TyKind::StringLit(_) => self.print_string_lit_ty(ty),
            ty::TyKind::Object(_) => self.print_object_ty(ty),
            ty::TyKind::NumberLit(_) => self.print_number_lit_ty(ty),
            ty::TyKind::BigIntLit(lit) => format!("{}n", self.c.atoms.get(lit.val)),
            ty::TyKind::Union(union) => union.tys.iter().fold(String::new(), |mut s, ty| {
                if !s.is_empty() {
                    s.push_str(" | ");
                }
                if ty.kind.is_object_anonymous()
                    && (!self.c.get_signatures_of_type(ty, SigKind::Call).is_empty()
                        || !self
                            .c
                            .get_signatures_of_type(ty, SigKind::Constructor)
                            .is_empty())
                {
                    let t = self.c.print_ty(ty, self.enclosing_declaration);
                    s.push_str(&format!("({t})",))
                } else {
                    s.push_str(self.c.print_ty(ty, self.enclosing_declaration))
                }
                s
            }),
            ty::TyKind::Intersection(i) => i.tys.iter().fold(String::new(), |mut s, ty| {
                if !s.is_empty() {
                    s.push_str(" & ");
                }
                if ty.kind.is_object_anonymous()
                    && (!self.c.get_signatures_of_type(ty, SigKind::Call).is_empty()
                        || !self
                            .c
                            .get_signatures_of_type(ty, SigKind::Constructor)
                            .is_empty())
                {
                    let t = self.c.print_ty(ty, self.enclosing_declaration);
                    s.push_str(&format!("({t})",))
                } else {
                    s.push_str(&self.c.print_ty(ty, self.enclosing_declaration))
                }
                s
            }),

            ty::TyKind::Param(param) => {
                let Some(symbol) = param.symbol else {
                    return "dummy_parameter".to_string();
                };
                let name = self.c.binder.symbol(symbol).name;
                self.c.atoms.get(name.expect_atom()).to_string()
            }
            ty::TyKind::IndexedAccess(n) => {
                let object = self
                    .c
                    .print_ty(n.object_ty, self.enclosing_declaration)
                    .to_string();
                let index = self.c.print_ty(n.index_ty, self.enclosing_declaration);
                format!("{object}[{index}]")
            }
            ty::TyKind::Cond(n) => {
                if let Some(symbol) = n.root.alias_symbol {
                    let name = self.c.binder.symbol(symbol).name;
                    self.c.atoms.get(name.expect_atom()).to_string()
                } else {
                    "cond".to_string()
                }
            }
            ty::TyKind::Index(n) => {
                let ty = self.c.print_ty(n.ty, self.enclosing_declaration);
                format!("keyof {ty}")
            }
            ty::TyKind::Intrinsic(i) => self.c.atoms.get(i.name).to_string(),
            ty::TyKind::Substitution(_) => "substitution".to_string(),
            ty::TyKind::StringMapping(s) => {
                let name = self.c.binder.symbol(s.symbol).name;
                self.c.atoms.get(name.expect_atom()).to_string()
            }
            ty::TyKind::TemplateLit(n) => {
                let mut s = String::with_capacity(32);
                s.push('`');
                for i in 0..n.texts.len() {
                    let text = n.texts[i];
                    s.push_str(self.c.atoms.get(text));
                    if let Some(ty) = n.tys.get(i) {
                        s.push_str(&format!(
                            "${{{}}}",
                            self.c.print_ty(ty, self.enclosing_declaration)
                        ));
                    }
                }
                s.push('`');
                s
            }
            ty::TyKind::UniqueESSymbol(_) => "unique es symbol".to_string(),
            ty::TyKind::Enum(n) => self.print_enum_symbol(n.symbol),
        }
    }

    fn print_string_lit_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> String {
        let lit = ty.kind.as_string_lit().unwrap();
        if ty.flags.contains(TypeFlags::ENUM_LITERAL) {
            let symbol = lit.symbol.unwrap();
            self.print_enum_lit_symbol(symbol)
        } else {
            format!("\"{}\"", self.c.atoms.get(lit.val))
        }
    }

    fn print_enum_symbol(&self, symbol: bolt_ts_binder::SymbolID) -> String {
        let name = self.c.binder.symbol(symbol).name;
        self.c.atoms.get(name.expect_atom()).to_string()
    }

    fn print_enum_lit_symbol(&mut self, symbol: bolt_ts_binder::SymbolID) -> String {
        let s = self.c.binder.symbol(symbol);
        let value_decl = s.value_decl.unwrap();
        let prop = self.c.atoms.get(s.name.expect_atom());
        let p = s.parent.unwrap();
        let p = self.c.binder.symbol(p);
        let object = self.c.atoms.get(p.name.expect_atom());
        let enum_member = self.c.p.node(value_decl).expect_enum_member();
        match enum_member.name {
            ast::EnumMemberNameKind::Ident(_) => {
                format!("{object}.{prop}")
            }
            ast::EnumMemberNameKind::StringLit { .. } => {
                format!("{object}[\"{prop}\"]")
            }
        }
    }

    fn print_number_lit_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> String {
        let lit = ty.kind.as_number_lit().unwrap();
        if ty.flags.contains(TypeFlags::ENUM_LITERAL) {
            let symbol = lit.symbol.unwrap();
            self.print_enum_lit_symbol(symbol)
        } else if lit.is(f64::INFINITY) {
            "Infinity".to_string()
        } else if lit.is(f64::NEG_INFINITY) {
            "-Infinity".to_string()
        } else {
            format!("{}", lit.val.val())
        }
    }

    fn print_object_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> String {
        let object_ty = ty.kind.as_object().unwrap();
        match object_ty.kind {
            ty::ObjectTyKind::Interface(i) => match i.symbol {
                Some(symbol) => {
                    let symbol_chain = self.lookup_symbol_chain(symbol);
                    self.print_symbol_chain(&symbol_chain)
                }
                _ => "{tuple_interface}".to_string(),
            },
            ty::ObjectTyKind::Anonymous(_) => self.print_anonymous_object_ty(ty),
            ty::ObjectTyKind::Tuple(ty) => {
                assert!(ty.element_flags.is_empty());
                "[]".to_string()
            }
            ty::ObjectTyKind::Reference(_) => self.print_reference_ty(ty),
            ty::ObjectTyKind::SingleSigTy(_) => "single signature type".to_string(),
            ty::ObjectTyKind::Mapped(m) => {
                if let Some(s) = m.alias_symbol {
                    let name = self.c.binder.symbol(s).name.expect_atom();
                    self.c.atoms.get(name).to_string()
                } else {
                    "mapped type".to_string()
                }
            }
            ty::ObjectTyKind::ReversedMapped(_) => todo!(),
        }
    }

    fn print_object_pat(&self, pat: &'cx ast::ObjectPat<'cx>) -> String {
        let mut res = String::from("{ ");
        for (idx, elem) in pat.elems.iter().enumerate() {
            match elem.name {
                ast::ObjectBindingName::Shorthand(n) => res.push_str(self.c.atoms.get(n.name)),
                ast::ObjectBindingName::Prop { prop_name, name } => {
                    match prop_name.kind {
                        ast::PropNameKind::Ident(n) => res.push_str(self.c.atoms.get(n.name)),
                        ast::PropNameKind::PrivateIdent(n) => {
                            res.push('#');
                            res.push_str(self.c.atoms.get(n.name))
                        }
                        ast::PropNameKind::StringLit { raw, .. } => {
                            res.push_str(self.c.atoms.get(raw.val))
                        }
                        ast::PropNameKind::NumLit(n) => res.push_str(&n.val.to_string()),
                        ast::PropNameKind::Computed(_) => res.push_str("[computed]"),
                        ast::PropNameKind::BigIntLit(_) => todo!(),
                    }
                    res.push_str(&": ");
                    res.push_str(&self.print_binding(name));
                }
            }
            if idx == pat.elems.len() - 1 {
                break;
            }
            res.push_str(", ");
        }
        res.push_str(" }");
        res
    }

    fn print_array_pat(&self, pat: &'cx ast::ArrayPat<'cx>) -> String {
        let mut res = String::from("[");
        for (i, elem) in pat.elems.iter().enumerate() {
            let is_last = i == pat.elems.len() - 1;
            match elem.kind {
                ast::ArrayBindingElemKind::Omit(_) => {
                    res.push(',');
                }
                ast::ArrayBindingElemKind::Binding(n) => {
                    if n.dotdotdot.is_some() {
                        res.push_str("...");
                    }
                    res.push_str(self.print_binding(n.name).as_str());
                    if !is_last {
                        res.push(',');
                    }
                }
            }
            if !is_last {
                res.push(' ');
            }
        }
        res.push_str("]");
        res
    }

    fn print_binding(&self, n: &'cx ast::Binding<'cx>) -> String {
        match n.kind {
            ast::BindingKind::Ident(n) => self.c.atoms.get(n.name).to_string(),
            ast::BindingKind::ObjectPat(pat) => self.print_object_pat(pat),
            ast::BindingKind::ArrayPat(pat) => self.print_array_pat(pat),
        }
    }

    fn print_anonymous_object_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> String {
        let a = ty.kind.expect_object_anonymous();
        let print_fn_like_str = |this: &mut Self, sig: &'cx super::Sig<'cx>| -> String {
            let params = sig
                .params
                .iter()
                .map(|param| {
                    let decl = this.c.get_symbol_decl(*param).unwrap();
                    let n = this.c.p.node(decl).expect_param_decl();
                    let name = this.print_binding(n.name);
                    let ty = this.c.get_type_of_symbol(*param);
                    let ty = this.c.print_ty(ty, this.enclosing_declaration);
                    format!("{name}: {ty}",)
                })
                .collect::<Vec<_>>()
                .join(", ");
            let ret = this.c.get_ret_ty_of_sig(sig);
            let ret = this.c.print_ty(ret, this.enclosing_declaration);
            format!("({params}) => {ret}")
        };
        let symbol_flags = a.symbol.map(|s| self.c.symbol(s).flags);
        if symbol_flags.is_some_and(|s| s.contains(SymbolFlags::OBJECT_LITERAL)) {
            let members = self
                .c
                .expect_ty_links(ty.id)
                .expect_structured_members()
                .members;

            let members = members
                .iter()
                .map(|(name, symbol)| {
                    let name = if let Some(name) = name.as_atom() {
                        let name = self.c.atoms.get(name).to_string();
                        if name.is_empty() {
                            "''".to_string()
                        } else {
                            name
                        }
                    } else if let Some(num) = name.as_numeric() {
                        num.to_string()
                    } else {
                        unreachable!()
                    };
                    let ty = self.c.get_type_of_symbol(*symbol);
                    format!(
                        "{}: {}; ",
                        name,
                        self.c.print_ty(ty, self.enclosing_declaration)
                    )
                })
                .collect::<Vec<_>>()
                .join("");
            format!("{{ {members}}}")
        } else if symbol_flags.is_some_and(|s| {
            s.intersects(
                SymbolFlags::CLASS
                    .union(SymbolFlags::VALUE_MODULE)
                    .union(SymbolFlags::CONST_ENUM),
            )
        }) {
            let s = self.c.symbol(a.symbol.unwrap());
            let name = s.name.expect_atom();
            format!("typeof {}", self.c.atoms.get(name))
        } else if let Some(sig) = self.c.get_signatures_of_type(ty, SigKind::Call).first() {
            print_fn_like_str(self, sig)
        } else if let Some(sig) = self
            .c
            .expect_ty_links(ty.id)
            .expect_structured_members()
            .ctor_sigs
            .first()
        {
            format!("new {}", print_fn_like_str(self, sig))
        } else if let Some(index_info) = self
            .c
            .expect_ty_links(ty.id)
            .expect_structured_members()
            .index_infos
            .first()
            && index_info.symbol != Symbol::ERR
        {
            let decl = self.c.binder.symbol(index_info.symbol).opt_decl().unwrap();
            let key_name = self.c.p.node(decl).expect_index_sig_decl().key;
            let key_ty = self
                .c
                .print_ty(index_info.key_ty, self.enclosing_declaration)
                .to_string();
            let val_ty = self
                .c
                .print_ty(index_info.val_ty, self.enclosing_declaration)
                .to_string();
            format!(
                "{{ [{key_name}: {key_ty}]: {val_ty} }}",
                key_name = self.print_binding(key_name),
            )
        } else {
            let members = self
                .c
                .expect_ty_links(ty.id)
                .expect_structured_members()
                .members;
            let members = members
                .iter()
                .map(|(name, symbol)| {
                    let ty = self.c.get_type_of_symbol(*symbol);
                    let field_name = name.to_string(&self.c.atoms);
                    let field_ty = self.c.print_ty(ty, self.enclosing_declaration);
                    format!("{field_name}: {field_ty}; ",)
                })
                .collect::<Vec<_>>()
                .join("");
            format!("{{ {members}}}")
        }
    }

    fn print_tuple_ty(&mut self, tuple: &'cx ty::TupleTy<'cx>, ty_args: ty::Tys<'cx>) -> String {
        format!(
            "{}[{}]",
            if tuple.readonly { "readonly " } else { "" },
            ty_args
                .iter()
                .enumerate()
                .map(|(i, t)| {
                    let flags = tuple.element_flags[i];
                    let t = self.c.print_ty(t, self.enclosing_declaration).to_string();
                    if flags.contains(ElementFlags::REQUIRED) {
                        t
                    } else if flags.contains(ElementFlags::OPTIONAL) {
                        format!("{t}?")
                    } else if flags.contains(ElementFlags::REST) {
                        format!("...{t}[]")
                    } else if flags.contains(ElementFlags::VARIADIC) {
                        format!("...{t}")
                    } else {
                        unreachable!()
                    }
                })
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    fn print_reference_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> String {
        let reference = ty.kind.expect_object_reference();
        let ty_args = self.c.get_ty_arguments(ty);
        if ty_args.is_empty() {
            return self
                .c
                .print_ty(reference.target, self.enclosing_declaration)
                .to_string();
        } else if let Some(tuple) = ty.as_tuple() {
            return self.print_tuple_ty(tuple, ty_args);
        }
        let args = ty_args
            .iter()
            .map(|ty| self.c.print_ty(ty, self.enclosing_declaration).to_string())
            .collect::<Vec<_>>()
            .join(", ");
        if let Some(i) = reference.interface_target() {
            format!("{}<{args}>", self.c.print_ty(i, self.enclosing_declaration))
        } else {
            unreachable!()
        }
    }

    fn lookup_symbol_chain(
        &self,
        symbol: bolt_ts_binder::SymbolID,
    ) -> Vec<bolt_ts_binder::SymbolID> {
        let mut res = vec![symbol];
        let mut current = symbol;
        while let Some(parent) = self.c.symbol(current).parent {
            res.push(parent);
            current = parent;
        }
        res.reverse();
        res
    }

    fn print_symbol_chain(&self, symbol_chain: &[bolt_ts_binder::SymbolID]) -> String {
        let mut res = String::new();
        for symbol in symbol_chain {
            let s = self.c.symbol(*symbol);
            let name = s.name.to_string(&self.c.atoms);
            if !res.is_empty() {
                res.push_str(".");
            }
            res.push_str(&name);
        }
        res
    }
}
