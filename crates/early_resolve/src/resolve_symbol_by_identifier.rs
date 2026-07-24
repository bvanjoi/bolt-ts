use bolt_ts_ast::r#trait::ClassLike;
use bolt_ts_ast::{self as ast, NodeFlags, keyword};
use bolt_ts_binder::{Symbol, SymbolFlags, SymbolID, SymbolName, SymbolTable};
use bolt_ts_config::Target;

use super::Resolver as R;

pub struct ResolvedResult {
    symbol: SymbolID,
    associated_declaration_for_containing_initializer_or_binding_name: Option<ast::NodeID>,
    within_deferred_context: bool,
    base_class_expression_cannot_reference_class_type_parameters: bool,
    property_with_invalid_initializer: Option<ast::NodeID>,
}

impl ResolvedResult {
    pub fn symbol(&self) -> SymbolID {
        self.symbol
    }
    pub fn associated_declaration_for_containing_initializer_or_binding_name(
        &self,
    ) -> Option<ast::NodeID> {
        self.associated_declaration_for_containing_initializer_or_binding_name
    }
    pub fn within_deferred_context(&self) -> bool {
        self.within_deferred_context
    }
    pub fn base_class_expression_cannot_reference_class_type_parameters(&self) -> bool {
        self.base_class_expression_cannot_reference_class_type_parameters
    }
    pub fn property_with_invalid_initializer(&self) -> Option<ast::NodeID> {
        self.property_with_invalid_initializer
    }
}

pub(super) fn get_symbol<'cx: 'a, 'a>(
    resolver: &impl Resolver<'cx, 'a>,
    symbols: &SymbolTable,
    name: SymbolName,
    meaning: SymbolFlags,
) -> Option<SymbolID> {
    if !meaning.is_empty()
        && let Some(symbol) = symbols.0.get(&name)
    {
        let symbol = resolver.get_merged_symbol(*symbol);
        let flags = resolver.symbol(symbol).flags;
        if flags.intersects(meaning) {
            return Some(symbol);
        } else if flags.contains(SymbolFlags::ALIAS) {
            // bound of parallel, handle this case in late_resolve
            return Some(symbol);
        }
    }
    None
}

fn get_is_deferred_context<'cx: 'a, 'a>(
    resolver: &impl Resolver<'cx, 'a>,
    location: ast::NodeID,
    last_location: Option<ast::NodeID>,
) -> bool {
    let last_location_is_fn_name =
        |name: ast::NodeID| last_location.is_some_and(|last_location| last_location == name);
    let l = resolver.node(location);
    match l {
        ast::Node::FnExpr(f) => {
            if f.name.is_some_and(|name| last_location_is_fn_name(name.id)) {
                return false;
            }
            if f.asterisk.is_some() || f.async_modifier.is_some() {
                return true;
            }
            resolver.get_immediately_invoked_fn_expr(location).is_none()
        }
        ast::Node::ArrowFnExpr(f) => {
            if f.async_modifier.is_some() {
                return true;
            }
            // TODO: name
            resolver.get_immediately_invoked_fn_expr(location).is_none()
        }
        ast::Node::TypeofTy(_) => true,
        ast::Node::ClassPropElem(n)
            if n.modifiers
                .is_none_or(|ms| !ms.flags.contains(ast::ModifierFlags::STATIC)) =>
        {
            last_location.is_none_or(|last_location| last_location != n.name.id())
        }
        ast::Node::FnDecl(ast::FnDecl { name, .. }) => last_location
            .is_none_or(|last_location| name.is_none_or(|name| name.id != last_location)),

        ast::Node::ClassMethodElem(ast::ClassMethodElem { name, .. })
        | ast::Node::ObjectMethodMember(ast::ObjectMethodMember { name, .. })
        | ast::Node::GetterDecl(ast::GetterDecl { name, .. })
        | ast::Node::SetterDecl(ast::SetterDecl { name, .. }) => {
            last_location.is_none_or(|last_location| name.id() != last_location)
        }
        _ => false,
    }
}

pub trait Resolver<'cx: 'a, 'a> {
    fn node(&self, id: ast::NodeID) -> ast::Node<'cx>;
    fn find_ancestor(
        &self,
        id: ast::NodeID,
        f: impl Fn(ast::NodeID) -> Option<bool>,
    ) -> Option<ast::NodeID>;
    fn get_immediately_invoked_fn_expr(&self, id: ast::NodeID) -> Option<&'cx ast::CallExpr<'cx>>;
    fn parent(&self, id: ast::NodeID) -> Option<ast::NodeID>;
    fn node_flags(&self, id: ast::NodeID) -> NodeFlags;
    fn locals(&self, id: ast::NodeID) -> Option<&SymbolTable>;
    fn get_merged_symbol(&self, symbol: SymbolID) -> SymbolID;
    fn symbol_of_decl(&self, id: ast::NodeID) -> SymbolID;
    fn symbol(&self, symbol: SymbolID) -> &Symbol;
    fn local_symbol(&self, id: ast::NodeID) -> Option<SymbolID>;
    fn is_external_or_commonjs_module(&self, id: ast::NodeID) -> bool;
    fn is_global_source_file(&self, id: ast::NodeID) -> bool;
    fn options(&self) -> &bolt_ts_config::NormalizedCompilerOptions;
    fn globals(&self) -> &SymbolTable;
}

impl<'cx, 'a> Resolver<'cx, 'a> for R<'cx, 'a, '_> {
    fn parent(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        self.parent(id)
    }
    fn node(&self, id: ast::NodeID) -> ast::Node<'cx> {
        self.p.node(id)
    }
    fn locals(&self, id: ast::NodeID) -> Option<&SymbolTable> {
        self.locals(id)
    }
    fn get_merged_symbol(&self, symbol: SymbolID) -> SymbolID {
        let symbols = &self.states[symbol.module().as_usize()].symbols;
        self.merged.get_merged_symbol(symbol, symbols)
    }
    fn symbol_of_decl(&self, id: bolt_ts_ast::NodeID) -> SymbolID {
        self.symbol_of_decl(id)
    }
    fn symbol(&self, symbol: SymbolID) -> &Symbol {
        self.symbol(symbol)
    }
    fn local_symbol(&self, id: bolt_ts_ast::NodeID) -> Option<SymbolID> {
        self.local_symbol(id)
    }
    fn is_global_source_file(&self, id: bolt_ts_ast::NodeID) -> bool {
        self.p.get(id.module()).is_global_source_file(id)
    }
    fn get_immediately_invoked_fn_expr(
        &self,
        id: bolt_ts_ast::NodeID,
    ) -> Option<&'cx ast::CallExpr<'cx>> {
        self.node_query().get_immediately_invoked_fn_expr(id)
    }
    fn find_ancestor(
        &self,
        id: bolt_ts_ast::NodeID,
        f: impl Fn(ast::NodeID) -> Option<bool>,
    ) -> Option<bolt_ts_ast::NodeID> {
        self.node_query().find_ancestor(id, f)
    }
    fn is_external_or_commonjs_module(&self, id: bolt_ts_ast::NodeID) -> bool {
        self.p.get(id.module()).is_external_or_commonjs_module()
    }
    fn node_flags(&self, id: bolt_ts_ast::NodeID) -> NodeFlags {
        self.p.node_flags(id)
    }
    fn options(&self) -> &bolt_ts_config::NormalizedCompilerOptions {
        self.options
    }
    fn globals(&self) -> &SymbolTable {
        self.globals
    }
}

pub fn resolve_symbol_by_ident<'a, 'cx: 'a>(
    resolver: &impl Resolver<'cx, 'a>,
    ident: &'cx ast::Ident,
    meaning: SymbolFlags,
) -> ResolvedResult {
    use ast::Node::*;
    let key = SymbolName::Atom(ident.name);
    let mut associated_declaration_for_containing_initializer_or_binding_name = None;
    let mut within_deferred_context = false;
    let mut last_location = Some(ident.id);
    let mut location = resolver.parent(ident.id);
    let mut property_with_invalid_initializer = None;

    while let Some(id) = location {
        // TODO: if ident.name == keyword::KW_CONST && is_const_assertion
        let n = resolver.node(id);
        if let Some(last) = last_location {
            match n {
                ast::Node::BlockModuleDecl(n) if n.name.id() == last => {
                    last_location = location;
                    location = resolver.parent(id);
                }
                ast::Node::NestedModuleDecl(n) if n.name.id == last => {
                    last_location = location;
                    location = resolver.parent(id);
                }
                ast::Node::EnumDecl(decl) if decl.name.id == last => {
                    last_location = location;
                    location = resolver.parent(id);
                }
                _ => {}
            }
        }
        let Some(id) = location else {
            break;
        };

        if let Some(locals) = resolver.locals(id)
            && !resolver.is_global_source_file(id)
            && let Some(symbol) = get_symbol(resolver, locals, key, meaning)
        {
            let res = resolver.symbol(symbol);
            let res_flags = res.flags;
            if res_flags.contains(SymbolFlags::ALIAS) {
                // handle this case in late_resolve
                return ResolvedResult {
                    symbol,
                    associated_declaration_for_containing_initializer_or_binding_name,
                    within_deferred_context,
                    base_class_expression_cannot_reference_class_type_parameters: false,
                    property_with_invalid_initializer,
                };
            }

            let mut use_result = true;
            let n = resolver.node(id);
            if n.is_fn_like()
                && let Some(last_location) = last_location
                && match n {
                    FnDecl(n) => n.body.is_none_or(|body| last_location != body.id),
                    ClassMethodElem(n) => n.body.is_none_or(|body| last_location != body.id),
                    _ => false, //TODO: other function decl,
                }
            {
                let flags = meaning.intersection(res_flags);
                if flags.intersects(SymbolFlags::TYPE) {
                    // TODO: last_location != JsDoc

                    use_result = if res_flags.contains(SymbolFlags::TYPE_PARAMETER) {
                        // TODO: last_location is synthesized
                        (match n {
                            FnDecl(f) => f.ty.is_some_and(|t| t.id() == last_location),
                            ClassMethodElem(n) => n.ty.is_some_and(|t| t.id() == last_location),
                            _ => false,
                        }) || matches!(
                            resolver.node(last_location),
                            ast::Node::ParamDecl(_) | ast::Node::TyParam(_)
                        )
                    } else {
                        false
                    };
                }
                if flags.intersects(SymbolFlags::VARIABLE)
                    && res_flags.contains(SymbolFlags::FUNCTION_SCOPED_VARIABLE)
                {
                    let last = resolver.node(last_location);
                    // TODO: last_location is synthesized
                    use_result = last.is_param_decl()
                        || (match n {
                            FnDecl(f) => f.ty.is_some_and(|t| t.id() == last_location),
                            ClassMethodElem(n) => n.ty.is_some_and(|t| t.id() == last_location),
                            _ => false,
                        } && res.value_decl.is_some_and(|n| {
                            resolver
                                .find_ancestor(n, |current| {
                                    resolver.node(current).is_param_decl().then_some(true)
                                })
                                .is_some()
                        }))
                };
            } else if let Some(cond) = n.as_cond_ty() {
                use_result = last_location.is_some_and(|last| last == cond.true_ty.id());
            }
            if use_result {
                return ResolvedResult {
                    symbol,
                    associated_declaration_for_containing_initializer_or_binding_name,
                    within_deferred_context,
                    base_class_expression_cannot_reference_class_type_parameters: false,
                    property_with_invalid_initializer,
                };
            }
        }
        within_deferred_context |= get_is_deferred_context(resolver, id, last_location);

        let n = resolver.node(id);
        match n {
            Program(_) if !resolver.is_external_or_commonjs_module(id) => (),
            Program(_) | NestedModuleDecl(_) | BlockModuleDecl(_) => {
                let symbol = resolver.symbol_of_decl(id);
                debug_assert!(symbol.module() == id.module());
                let symbol = resolver.get_merged_symbol(symbol);
                let module_exports = &resolver.symbol(symbol).exports();
                let mut stop = false;
                if match n {
                    Program(_) => true,
                    NestedModuleDecl(n) => resolver.node_flags(n.id).intersects(NodeFlags::AMBIENT),
                    BlockModuleDecl(n) => {
                        !n.is_global_argument
                            && resolver.node_flags(n.id).intersects(NodeFlags::AMBIENT)
                    }
                    _ => unreachable!(),
                } {
                    if let Some(result) = module_exports
                        .and_then(|table| table.0.get(&SymbolName::ExportDefault))
                        .copied()
                    {
                        let r = resolver.symbol(result);
                        debug_assert!(result.module() == id.module());
                        if r.flags.intersects(meaning)
                            && let Some(local_symbol) =
                                get_local_symbol_for_export_default(resolver, result)
                            && let l = resolver.symbol(local_symbol)
                            && l.name.as_atom().is_some_and(|name| name == ident.name)
                        {
                            return ResolvedResult {
                                symbol: result,
                                associated_declaration_for_containing_initializer_or_binding_name,
                                within_deferred_context,
                                base_class_expression_cannot_reference_class_type_parameters: false,
                                property_with_invalid_initializer,
                            };
                        }
                    }
                    // TODO: default
                    if let Some(module_exports) = module_exports
                        && let Some(module_export) = module_exports.0.get(&key).copied()
                        && let s = resolver.symbol(module_export)
                        && s.flags == SymbolFlags::ALIAS
                        && s.get_declaration_of_kind(|n| {
                            matches!(
                                resolver.node(n),
                                ast::Node::ExportNamedSpec(_)
                                    | ast::Node::ExportShorthandSpec(_)
                                    | ast::Node::NsExport(_)
                            )
                        })
                        .is_some()
                    {
                        stop = true;
                    }
                }

                if !stop
                    && ident.name != keyword::KW_DEFAULT
                    && let Some(symbols) = module_exports
                    && let Some(module_export) = get_symbol(
                        resolver,
                        symbols,
                        key,
                        meaning.intersection(SymbolFlags::MODULE_MEMBER),
                    )
                {
                    // TODO: is_source_file
                    return ResolvedResult {
                        symbol: module_export,
                        associated_declaration_for_containing_initializer_or_binding_name,
                        within_deferred_context,
                        base_class_expression_cannot_reference_class_type_parameters: false,
                        property_with_invalid_initializer,
                    };
                }
            }
            EnumDecl(_) => {
                if let Some(exports) = resolver.symbol(resolver.symbol_of_decl(id)).exports()
                    && let Some(res) =
                        get_symbol(resolver, exports, key, meaning & SymbolFlags::ENUM_MEMBER)
                {
                    return ResolvedResult {
                        symbol: res,
                        associated_declaration_for_containing_initializer_or_binding_name,
                        within_deferred_context,
                        base_class_expression_cannot_reference_class_type_parameters: false,
                        property_with_invalid_initializer,
                    };
                }
            }
            ClassPropElem(n) => {
                if let Some(location) = location
                    && !n
                        .modifiers
                        .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::STATIC))
                    && let parent_id = resolver.parent(location).unwrap()
                    && let parent = resolver.node(parent_id)
                    && let Some(ctor) = parent
                        .as_class_decl()
                        .and_then(|c| c.find_ctor_decl())
                        .or_else(|| parent.as_class_expr().and_then(|c| c.find_ctor_decl()))
                    && let ctor_id = ctor.id
                    && let Some(locals) = resolver.locals(ctor_id)
                    && get_symbol(resolver, locals, key, meaning & SymbolFlags::VALUE).is_some()
                {
                    property_with_invalid_initializer = Some(n.id);
                }
            }
            ClassDecl(_) | ClassExpr(_) | InterfaceDecl(_) => {
                if let Some(res) = resolver
                    .symbol(resolver.symbol_of_decl(id))
                    .members()
                    .and_then(|m| m.0.get(&key))
                    .copied()
                    && resolver
                        .symbol(res)
                        .flags
                        .intersects(meaning & SymbolFlags::TYPE)
                {
                    if !is_type_param_symbol_declared_in_container(resolver, res, id) {
                        break;
                    }
                    // TODO: last location
                    return ResolvedResult {
                        symbol: res,
                        associated_declaration_for_containing_initializer_or_binding_name,
                        within_deferred_context,
                        base_class_expression_cannot_reference_class_type_parameters: false,
                        property_with_invalid_initializer,
                    };
                }
                if let Some(c) = n.as_class_expr()
                    && meaning.intersects(SymbolFlags::CLASS)
                    && c.name.is_some_and(|n| n.name == ident.name)
                {
                    return ResolvedResult {
                        symbol: resolver.symbol_of_decl(id),
                        associated_declaration_for_containing_initializer_or_binding_name,
                        within_deferred_context,
                        base_class_expression_cannot_reference_class_type_parameters: false,
                        property_with_invalid_initializer,
                    };
                }
            }
            ExprWithTyArgs(expr) => {
                if last_location.is_some_and(|l| l == expr.expr.id())
                    && let parent_id = resolver.parent(id).unwrap()
                    && resolver.node(parent_id).is_class_extends_clause()
                {
                    let container = resolver.parent(parent_id).unwrap();
                    let c = resolver.node(container);
                    if c.is_class_like()
                        && let symbol = resolver.symbol_of_decl(container)
                        && let Some(members) = resolver.symbol(symbol).members()
                        && let Some(res) =
                            get_symbol(resolver, members, key, meaning & SymbolFlags::TYPE)
                    {
                        debug_assert!(!resolver.symbol(res).flags.contains(SymbolFlags::ALIAS));
                        return ResolvedResult {
                            symbol: Symbol::ERR,
                            associated_declaration_for_containing_initializer_or_binding_name,
                            within_deferred_context,
                            base_class_expression_cannot_reference_class_type_parameters: true,
                            property_with_invalid_initializer,
                        };
                    }
                }
            }
            ComputedPropName(_) => {
                let grand_parent_id = resolver.parent(resolver.parent(id).unwrap()).unwrap();
                let grand_parent = resolver.node(grand_parent_id);
                if (grand_parent.is_class_like() || grand_parent.is_interface_decl())
                    && let symbol = resolver.symbol_of_decl(grand_parent_id)
                    && let Some(members) = resolver.symbol(symbol).members()
                    && let Some(res) =
                        get_symbol(resolver, members, key, meaning & SymbolFlags::TYPE)
                {
                    debug_assert!(!resolver.symbol(res).flags.contains(SymbolFlags::ALIAS));
                    return ResolvedResult {
                        symbol: Symbol::ERR,
                        associated_declaration_for_containing_initializer_or_binding_name,
                        within_deferred_context,
                        base_class_expression_cannot_reference_class_type_parameters: true,
                        property_with_invalid_initializer,
                    };
                }
            }
            ArrowFnExpr(_) if *resolver.options().target() >= Target::ES2015 => {}
            ArrowFnExpr(_) | ClassMethodElem(_) | ClassCtor(_) | GetterDecl(_) | SetterDecl(_)
            | FnDecl(_) => {
                if meaning.intersects(SymbolFlags::VARIABLE)
                    && ident.name == keyword::IDENT_ARGUMENTS
                {
                    return ResolvedResult {
                        symbol: Symbol::ARGUMENTS,
                        associated_declaration_for_containing_initializer_or_binding_name,
                        within_deferred_context,
                        base_class_expression_cannot_reference_class_type_parameters: false,
                        property_with_invalid_initializer,
                    };
                }
            }
            FnExpr(f) => {
                if meaning.intersects(SymbolFlags::VARIABLE)
                    && ident.name == keyword::IDENT_ARGUMENTS
                {
                    return ResolvedResult {
                        symbol: Symbol::ARGUMENTS,
                        associated_declaration_for_containing_initializer_or_binding_name,
                        within_deferred_context,
                        base_class_expression_cannot_reference_class_type_parameters: false,
                        property_with_invalid_initializer,
                    };
                }
                if meaning.contains(SymbolFlags::FUNCTION)
                    && f.name.is_some_and(|n| n.name == ident.name)
                {
                    return ResolvedResult {
                        symbol: resolver.symbol_of_decl(id),
                        associated_declaration_for_containing_initializer_or_binding_name,
                        within_deferred_context,
                        base_class_expression_cannot_reference_class_type_parameters: false,
                        property_with_invalid_initializer,
                    };
                }
            }
            ParamDecl(p) => {
                if let Some(last_location) = last_location
                    && (p.init.is_some_and(|init| init.id() == last_location)
                        || match p.name.kind {
                            ast::BindingKind::Ident(_) => false,
                            ast::BindingKind::ObjectPat(n) => n.id == last_location,
                            ast::BindingKind::ArrayPat(n) => n.id == last_location,
                        })
                    && associated_declaration_for_containing_initializer_or_binding_name.is_none()
                {
                    associated_declaration_for_containing_initializer_or_binding_name = Some(id);
                }
            }
            _ => {}
        }
        last_location = location;
        location = resolver.parent(id);
    }

    if let Some(symbol) = get_symbol(resolver, resolver.globals(), key, meaning) {
        return ResolvedResult {
            symbol,
            associated_declaration_for_containing_initializer_or_binding_name,
            within_deferred_context,
            base_class_expression_cannot_reference_class_type_parameters: false,
            property_with_invalid_initializer,
        };
    } else if ident.name == keyword::IDENT_GLOBAL_THIS && meaning.intersects(SymbolFlags::MODULE) {
        return ResolvedResult {
            symbol: Symbol::GLOBAL_THIS,
            associated_declaration_for_containing_initializer_or_binding_name,
            within_deferred_context,
            base_class_expression_cannot_reference_class_type_parameters: false,
            property_with_invalid_initializer,
        };
    }

    ResolvedResult {
        symbol: Symbol::ERR,
        associated_declaration_for_containing_initializer_or_binding_name,
        within_deferred_context,
        base_class_expression_cannot_reference_class_type_parameters: false,
        property_with_invalid_initializer,
    }
}

fn get_local_symbol_for_export_default<'cx: 'a, 'a>(
    resolver: &impl Resolver<'cx, 'a>,
    symbol: SymbolID,
) -> Option<SymbolID> {
    let s = resolver.symbol(symbol);
    let decls = s.decls.as_ref()?;
    for decl in decls {
        if let Some(local_symbol) = resolver.local_symbol(*decl) {
            return Some(local_symbol);
        }
    }
    None
}

fn is_type_param_symbol_declared_in_container<'cx: 'a, 'a>(
    resolver: &impl Resolver<'cx, 'a>,
    symbol: SymbolID,
    container: ast::NodeID,
) -> bool {
    let Some(decls) = &resolver.symbol(symbol).decls else {
        return false;
    };
    for decl in decls {
        let decl = *decl;
        if resolver.node(decl).is_ty_param() {
            // TODO: js doc template tag
            let parent = resolver.parent(decl);
            if let Some(parent) = parent
                && parent == container
            {
                // TODO: js doc template tag
                return true;
            }
        }
    }
    false
}
