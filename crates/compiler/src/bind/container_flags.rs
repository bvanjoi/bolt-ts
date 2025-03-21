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
    fn get_container_flags(&self, p: &parser::ParseResult) -> ContainerFlags;
}

macro_rules! container_flags_for_node {
  ($(($node_kind:ident, $flags: expr)),* $(,)?) => {
      $(
          impl GetContainerFlags for bolt_ts_ast::$node_kind<'_> {
              fn get_container_flags(&self, _: &parser::ParseResult) -> ContainerFlags {
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
    (TypeDecl, C_AND_L),
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
    fn get_container_flags(&self, p: &parser::ParseResult) -> ContainerFlags {
        if p.is_object_lit_or_class_expr_method_or_accessor(self.id) {
            C_AND_L_AND_CF_AND_F_AND_O
        } else {
            C_AND_L_AND_CF_AND_F
        }
    }
}

impl GetContainerFlags for bolt_ts_ast::SetterDecl<'_> {
    fn get_container_flags(&self, p: &parser::ParseResult) -> ContainerFlags {
        if p.is_object_lit_or_class_expr_method_or_accessor(self.id) {
            C_AND_L_AND_CF_AND_F_AND_O
        } else {
            C_AND_L_AND_CF_AND_F
        }
    }
}

impl GetContainerFlags for bolt_ts_ast::ClassMethodElem<'_> {
    fn get_container_flags(&self, p: &parser::ParseResult) -> ContainerFlags {
        if p.is_object_lit_or_class_expr_method_or_accessor(self.id) {
            C_AND_L_AND_CF_AND_F_AND_O
        } else {
            C_AND_L_AND_CF_AND_F
        }
    }
}

impl GetContainerFlags for bolt_ts_ast::ClassPropElem<'_> {
    fn get_container_flags(&self, _: &parser::ParseResult) -> ContainerFlags {
        if self.init.is_some() {
            ContainerFlags::IS_CONTROL_FLOW_CONTAINER
        } else {
            ContainerFlags::empty()
        }
    }
}

impl GetContainerFlags for bolt_ts_ast::BlockStmt<'_> {
    fn get_container_flags(&self, p: &parser::ParseResult) -> ContainerFlags {
        let parent = p.parent(self.id).unwrap();
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
    node: bolt_ts_ast::NodeID,
) -> ContainerFlags {
    let n = p.node(node);
    use bolt_ts_ast::Node::*;
    match n {
        ClassExpr(n) => n.get_container_flags(p),
        ClassDecl(n) => n.get_container_flags(p),
        EnumDecl(n) => n.get_container_flags(p),
        ObjectLit(n) => n.get_container_flags(p),
        ObjectLitTy(n) => n.get_container_flags(p),
        InterfaceDecl(n) => n.get_container_flags(p),
        NamespaceDecl(n) => n.get_container_flags(p),
        TypeDecl(n) => n.get_container_flags(p),
        MappedTy(n) => n.get_container_flags(p),
        IndexSigDecl(n) => n.get_container_flags(p),
        Program(n) => n.get_container_flags(p),
        ObjectMethodMember(n) => n.get_container_flags(p),
        ClassCtor(n) => n.get_container_flags(p),
        FnDecl(n) => n.get_container_flags(p),
        MethodSignature(n) => n.get_container_flags(p),
        CallSigDecl(n) => n.get_container_flags(p),
        FnTy(n) => n.get_container_flags(p),
        CtorSigDecl(n) => n.get_container_flags(p),
        CtorTy(n) => n.get_container_flags(p),
        FnExpr(n) => n.get_container_flags(p),
        ArrowFnExpr(n) => n.get_container_flags(p),
        ModuleBlock(n) => n.get_container_flags(p),
        CatchClause(n) => n.get_container_flags(p),
        ForStmt(n) => n.get_container_flags(p),
        ForInStmt(n) => n.get_container_flags(p),
        ForOfStmt(n) => n.get_container_flags(p),
        GetterDecl(n) => n.get_container_flags(p),
        SetterDecl(n) => n.get_container_flags(p),
        ClassMethodElem(n) => n.get_container_flags(p),
        ClassPropElem(n) => n.get_container_flags(p),
        BlockStmt(n) => n.get_container_flags(p),
        _ => ContainerFlags::empty(),
    }
}
