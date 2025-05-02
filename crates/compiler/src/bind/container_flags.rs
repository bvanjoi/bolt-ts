use super::NodeQuery;
use crate::parser;

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
        p: &parser::ParseResult,
        parent_map: &super::ParentMap,
    ) -> ContainerFlags;
}

macro_rules! container_flags_for_node {
  ($(($node_kind:ident, $flags: expr)),* $(,)?) => {
      $(
          impl GetContainerFlags for bolt_ts_ast::$node_kind<'_> {
              fn get_container_flags(&self, _: &parser::ParseResult, _: &super::ParentMap) -> ContainerFlags {
                  $flags
              }
          }
      )*
  };
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

container_flags_for_node!(
    (ClassExpr, ContainerFlags::IS_CONTAINER),
    (ClassDecl, ContainerFlags::IS_CONTAINER),
    (EnumDecl, ContainerFlags::IS_CONTAINER),
    (ObjectLit, ContainerFlags::IS_CONTAINER),
    (ObjectLitTy, ContainerFlags::IS_CONTAINER),
    // TODO: JsDocTypeLiteral
    // TODO: JSX
    // ------
    (
        InterfaceDecl,
        ContainerFlags::IS_CONTAINER | ContainerFlags::IS_INTERFACE
    ),
    // ------
    (NsDecl, C_AND_L),
    (TypeAliasDecl, C_AND_L),
    (MappedTy, C_AND_L),
    (IndexSigDecl, C_AND_L),
    // ------
    (Program, C_AND_L_AND_CF),
    // ------
    (ObjectMethodMember, C_AND_L_AND_CF_AND_F_AND_O),
    // ------
    (ClassCtor, C_AND_L_AND_CF_AND_F),
    (FnDecl, C_AND_L_AND_CF_AND_F),
    (MethodSignature, C_AND_L_AND_CF_AND_F),
    (CallSigDecl, C_AND_L_AND_CF_AND_F),
    // TODO: js doc sig
    // TODO: js doc fn
    (FnTy, C_AND_L_AND_CF_AND_F),
    (CtorSigDecl, C_AND_L_AND_CF_AND_F),
    (CtorTy, C_AND_L_AND_CF_AND_F),
    // TODO: class static block
    // -----
    (FnExpr, C_AND_L_AND_CF_AND_F_AND_FE),
    (ArrowFnExpr, C_AND_L_AND_CF_AND_F_AND_FE),
    // ----
    (ModuleBlock, ContainerFlags::IS_CONTROL_FLOW_CONTAINER),
    // ----
    (CatchClause, BS_AND_L),
    (ForStmt, BS_AND_L),
    (ForInStmt, BS_AND_L),
    (ForOfStmt, BS_AND_L),
    // TODO: case
);

impl GetContainerFlags for bolt_ts_ast::GetterDecl<'_> {
    fn get_container_flags(
        &self,
        p: &parser::ParseResult,
        parent_map: &super::ParentMap,
    ) -> ContainerFlags {
        let nq = super::BinderNodeQuery::new(parent_map, p);
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
        p: &parser::ParseResult,
        parent_map: &super::ParentMap,
    ) -> ContainerFlags {
        let nq = super::BinderNodeQuery::new(parent_map, p);
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
        p: &parser::ParseResult,
        parent_map: &super::ParentMap,
    ) -> ContainerFlags {
        let nq = super::BinderNodeQuery::new(parent_map, p);
        if nq.is_object_lit_or_class_expr_method_or_accessor(self.id) {
            C_AND_L_AND_CF_AND_F_AND_O
        } else {
            C_AND_L_AND_CF_AND_F
        }
    }
}

impl GetContainerFlags for bolt_ts_ast::ClassPropElem<'_> {
    fn get_container_flags(&self, _: &parser::ParseResult, _: &super::ParentMap) -> ContainerFlags {
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
        p: &parser::ParseResult,
        parent_map: &super::ParentMap,
    ) -> ContainerFlags {
        let parent = parent_map.parent_unfinished(self.id).unwrap();
        let parent = p.node(parent);
        if parent.is_fn_like() || parent.is_class_static_block_decl() {
            ContainerFlags::empty()
        } else {
            BS_AND_L
        }
    }
}

pub(super) fn container_flags_for_node(
    p: &parser::ParseResult,
    parent_map: &super::ParentMap,
    node: bolt_ts_ast::NodeID,
) -> ContainerFlags {
    let n = p.node(node);
    use bolt_ts_ast::Node::*;
    match n {
        ClassExpr(n) => n.get_container_flags(p, parent_map),
        ClassDecl(n) => n.get_container_flags(p, parent_map),
        EnumDecl(n) => n.get_container_flags(p, parent_map),
        ObjectLit(n) => n.get_container_flags(p, parent_map),
        ObjectLitTy(n) => n.get_container_flags(p, parent_map),
        InterfaceDecl(n) => n.get_container_flags(p, parent_map),
        NamespaceDecl(n) => n.get_container_flags(p, parent_map),
        TypeAliasDecl(n) => n.get_container_flags(p, parent_map),
        MappedTy(n) => n.get_container_flags(p, parent_map),
        IndexSigDecl(n) => n.get_container_flags(p, parent_map),
        Program(n) => n.get_container_flags(p, parent_map),
        ObjectMethodMember(n) => n.get_container_flags(p, parent_map),
        ClassCtor(n) => n.get_container_flags(p, parent_map),
        FnDecl(n) => n.get_container_flags(p, parent_map),
        MethodSignature(n) => n.get_container_flags(p, parent_map),
        CallSigDecl(n) => n.get_container_flags(p, parent_map),
        FnTy(n) => n.get_container_flags(p, parent_map),
        CtorSigDecl(n) => n.get_container_flags(p, parent_map),
        CtorTy(n) => n.get_container_flags(p, parent_map),
        FnExpr(n) => n.get_container_flags(p, parent_map),
        ArrowFnExpr(n) => n.get_container_flags(p, parent_map),
        ModuleBlock(n) => n.get_container_flags(p, parent_map),
        CatchClause(n) => n.get_container_flags(p, parent_map),
        ForStmt(n) => n.get_container_flags(p, parent_map),
        ForInStmt(n) => n.get_container_flags(p, parent_map),
        ForOfStmt(n) => n.get_container_flags(p, parent_map),
        GetterDecl(n) => n.get_container_flags(p, parent_map),
        SetterDecl(n) => n.get_container_flags(p, parent_map),
        ClassMethodElem(n) => n.get_container_flags(p, parent_map),
        ClassPropElem(n) => n.get_container_flags(p, parent_map),
        BlockStmt(n) => n.get_container_flags(p, parent_map),
        _ => ContainerFlags::empty(),
    }
}
