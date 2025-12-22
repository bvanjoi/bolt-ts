use super::NodeQuery;

bitflags::bitflags! {
  #[derive(Clone, Copy, Debug, Default)]
  pub struct ContainerFlags: u8 {
    const IS_CONTAINER = 1 << 0;
    const IS_BLOCK_SCOPED_CONTAINER = 1 << 1;
    const IS_CONTROL_FLOW_CONTAINER = 1 << 2;
    const IS_FUNCTION_LIKE = 1 << 3;
    const IS_FUNCTION_EXPRESSION = 1 << 4;
    const HAS_LOCALS = 1 << 5;
    const IS_INTERFACE = 1 << 6;
    const IS_OBJECT_LITERAL_OR_CLASS_EXPRESSION_METHOD_OR_ACCESSOR = 1 << 7;
  }
}

trait GetContainerFlags {
    fn get_container_flags(
        &self,
        p: &bolt_ts_parser::ParseResultForGraph,
        parent_map: &super::ParentMap,
    ) -> ContainerFlags;
}

/// `IS_CONTAINER | HAS_LOCALS`
const C_AND_L: ContainerFlags = ContainerFlags::IS_CONTAINER.union(ContainerFlags::HAS_LOCALS);

/// `IS_CONTAINER | HAS_LOCALS | IS_CONTROL_FLOW_CONTAINER`
const C_AND_L_AND_CF: ContainerFlags = C_AND_L.union(ContainerFlags::IS_CONTROL_FLOW_CONTAINER);

/// `IS_CONTAINER | HAS_LOCALS | IS_CONTROL_FLOW_CONTAINER | IS_FUNCTION_LIKE`
const C_AND_L_AND_CF_AND_F: ContainerFlags = C_AND_L_AND_CF.union(ContainerFlags::IS_FUNCTION_LIKE);

/// `IS_CONTAINER | HAS_LOCALS | IS_CONTROL_FLOW_CONTAINER | IS_FUNCTION_LIKE | IS_OBJECT_LITERAL_OR_CLASS_EXPRESSION_METHOD_OR_ACCESSOR`
const C_AND_L_AND_CF_AND_F_AND_O: ContainerFlags = C_AND_L_AND_CF_AND_F
    .union(ContainerFlags::IS_OBJECT_LITERAL_OR_CLASS_EXPRESSION_METHOD_OR_ACCESSOR);

/// `IS_CONTAINER | HAS_LOCALS | IS_CONTROL_FLOW_CONTAINER | IS_FUNCTION_LIKE | IS_FUNCTION_EXPRESSION`
const C_AND_L_AND_CF_AND_F_AND_FE: ContainerFlags =
    C_AND_L_AND_CF_AND_F.union(ContainerFlags::IS_FUNCTION_EXPRESSION);

/// `IS_BLOCK_SCOPED_CONTAINER | HAS_LOCALS`
const BS_AND_L: ContainerFlags =
    ContainerFlags::IS_BLOCK_SCOPED_CONTAINER.union(ContainerFlags::HAS_LOCALS);

impl GetContainerFlags for bolt_ts_ast::GetterDecl<'_> {
    fn get_container_flags(
        &self,
        p: &bolt_ts_parser::ParseResultForGraph,
        parent_map: &super::ParentMap,
    ) -> ContainerFlags {
        let nq = NodeQuery::new(parent_map, p);
        if nq.is_object_lit_or_class_expr_method_or_accessor(self.id) {
            C_AND_L_AND_CF_AND_F_AND_O
        } else {
            C_AND_L_AND_CF_AND_F
        }
    }
}

impl GetContainerFlags for bolt_ts_ast::SetterDecl<'_> {
    fn get_container_flags(
        &self,
        p: &bolt_ts_parser::ParseResultForGraph,
        parent_map: &super::ParentMap,
    ) -> ContainerFlags {
        let nq = NodeQuery::new(parent_map, p);
        if nq.is_object_lit_or_class_expr_method_or_accessor(self.id) {
            C_AND_L_AND_CF_AND_F_AND_O
        } else {
            C_AND_L_AND_CF_AND_F
        }
    }
}

impl GetContainerFlags for bolt_ts_ast::ClassMethodElem<'_> {
    fn get_container_flags(
        &self,
        p: &bolt_ts_parser::ParseResultForGraph,
        parent_map: &super::ParentMap,
    ) -> ContainerFlags {
        let nq = NodeQuery::new(parent_map, p);
        if nq.is_object_lit_or_class_expr_method_or_accessor(self.id) {
            C_AND_L_AND_CF_AND_F_AND_O
        } else {
            C_AND_L_AND_CF_AND_F
        }
    }
}

impl GetContainerFlags for bolt_ts_ast::ClassPropElem<'_> {
    fn get_container_flags(
        &self,
        _: &bolt_ts_parser::ParseResultForGraph,
        _: &super::ParentMap,
    ) -> ContainerFlags {
        if self.init.is_some() {
            ContainerFlags::IS_CONTROL_FLOW_CONTAINER
        } else {
            ContainerFlags::empty()
        }
    }
}

impl GetContainerFlags for bolt_ts_ast::BlockStmt<'_> {
    fn get_container_flags(
        &self,
        p: &bolt_ts_parser::ParseResultForGraph,
        parent_map: &super::ParentMap,
    ) -> ContainerFlags {
        let parent = parent_map.parent(self.id).unwrap();
        let parent = p.node(parent);
        if parent.is_fn_like() || parent.is_class_static_block_decl() {
            ContainerFlags::empty()
        } else {
            BS_AND_L
        }
    }
}

pub(super) fn container_flags_for_node(
    p: &bolt_ts_parser::ParseResultForGraph,
    parent_map: &super::ParentMap,
    node: bolt_ts_ast::NodeID,
) -> ContainerFlags {
    let n = p.node(node);
    use bolt_ts_ast::Node::*;
    match n {
        ClassExpr(_) | ClassDecl(_) | EnumDecl(_) | ObjectLit(_) | ObjectLitTy(_) => {
            ContainerFlags::IS_CONTAINER
        }
        // TODO: JsDocTypeLiteral
        // TODO: JSX
        InterfaceDecl(_) => ContainerFlags::IS_CONTAINER.union(ContainerFlags::IS_INTERFACE),
        ModuleDecl(_) | TypeAliasDecl(_) | MappedTy(_) | IndexSigDecl(_) => C_AND_L,
        Program(_) => C_AND_L_AND_CF,
        ObjectMethodMember(_) => C_AND_L_AND_CF_AND_F_AND_O,
        ClassCtor(_) | FnDecl(_) | MethodSignature(_) | CallSigDecl(_) => C_AND_L_AND_CF_AND_F,
        // TODO: js doc sig
        // TODO: js doc fn
        FnTy(_) | CtorSigDecl(_) | CtorTy(_) | ClassStaticBlockDecl(_) => C_AND_L_AND_CF_AND_F,
        FnExpr(_) | ArrowFnExpr(_) => C_AND_L_AND_CF_AND_F_AND_FE,
        ModuleBlock(_) => ContainerFlags::IS_CONTROL_FLOW_CONTAINER,
        CatchClause(_) | ForStmt(_) | ForInStmt(_) | ForOfStmt(_) | CaseBlock(_) => BS_AND_L,
        GetterDecl(n) => n.get_container_flags(p, parent_map),
        SetterDecl(n) => n.get_container_flags(p, parent_map),
        ClassMethodElem(n) => n.get_container_flags(p, parent_map),
        ClassPropElem(n) => n.get_container_flags(p, parent_map),
        BlockStmt(n) => n.get_container_flags(p, parent_map),
        _ => ContainerFlags::empty(),
    }
}
