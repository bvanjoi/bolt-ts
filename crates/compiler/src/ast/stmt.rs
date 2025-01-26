use super::*;
use crate::keyword;

pub type Stmts<'cx> = &'cx [&'cx Stmt<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct Stmt<'cx> {
    pub kind: StmtKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum StmtKind<'cx> {
    Empty(&'cx EmptyStmt),
    Var(&'cx VarStmt<'cx>),
    If(&'cx IfStmt<'cx>),
    For(&'cx ForStmt<'cx>),
    ForOf(&'cx ForOfStmt<'cx>),
    ForIn(&'cx ForInStmt<'cx>),
    Break(&'cx BreakStmt<'cx>),
    Continue(&'cx ContinueStmt<'cx>),
    Return(&'cx RetStmt<'cx>),
    Block(&'cx BlockStmt<'cx>),
    Fn(&'cx FnDecl<'cx>),
    Class(&'cx ClassDecl<'cx>),
    Expr(&'cx super::Expr<'cx>),
    Interface(&'cx InterfaceDecl<'cx>),
    Type(&'cx TypeDecl<'cx>),
    Namespace(&'cx NsDecl<'cx>),
    Throw(&'cx ThrowStmt<'cx>),
    Enum(&'cx EnumDecl<'cx>),
    Import(&'cx ImportDecl<'cx>),
    Export(&'cx ExportDecl<'cx>),
    Try(&'cx TryStmt<'cx>),
    While(&'cx WhileStmt<'cx>),
    Do(&'cx DoStmt<'cx>),
}

impl Stmt<'_> {
    pub fn id(&self) -> NodeID {
        use StmtKind::*;
        match self.kind {
            Empty(empty) => empty.id,
            Var(var) => var.id,
            If(if_) => if_.id,
            Return(ret) => ret.id,
            Block(block) => block.id,
            Fn(f) => f.id,
            Class(c) => c.id,
            Expr(expr) => expr.id(),
            Interface(i) => i.id,
            Type(t) => t.id,
            Namespace(n) => n.id,
            Throw(t) => t.id,
            Enum(e) => e.id,
            Import(n) => n.id,
            Export(n) => n.id,
            For(n) => n.id,
            ForOf(n) => n.id,
            ForIn(n) => n.id,
            Break(n) => n.id,
            Continue(n) => n.id,
            Try(n) => n.id,
            While(n) => n.id,
            Do(n) => n.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DoStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub stmt: &'cx Stmt<'cx>,
    pub expr: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct WhileStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
    pub stmt: &'cx Stmt<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct TryStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub try_block: &'cx BlockStmt<'cx>,
    pub catch_clause: Option<&'cx CatchClause<'cx>>,
    pub finally_block: Option<&'cx BlockStmt<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CatchClause<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub var: Option<&'cx VarDecl<'cx>>,
    pub block: &'cx BlockStmt<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct BreakStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub label: Option<&'cx Ident>,
}

#[derive(Debug, Clone, Copy)]
pub struct ContinueStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub label: Option<&'cx Ident>,
}

#[derive(Debug, Clone, Copy)]
pub struct ForStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub init: Option<ForInitKind<'cx>>,
    pub cond: Option<&'cx Expr<'cx>>,
    pub incr: Option<&'cx Expr<'cx>>,
    pub body: &'cx Stmt<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ForInitKind<'cx> {
    Var((VarKind, VarDecls<'cx>)),
    Expr(&'cx Expr<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct ForOfStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub r#await: Option<Span>,
    pub init: ForInitKind<'cx>,
    pub expr: &'cx Expr<'cx>,
    pub body: &'cx Stmt<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ForInStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub init: ForInitKind<'cx>,
    pub expr: &'cx Expr<'cx>,
    pub body: &'cx Stmt<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct EnumDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: &'cx Ident,
    pub members: EnumMembers<'cx>,
}

pub type EnumMembers<'cx> = &'cx [&'cx EnumMember<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct EnumMember<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx PropName<'cx>,
    pub init: Option<&'cx Expr<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ThrowStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct NsDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: ModuleName<'cx>,
    pub block: Option<&'cx BlockStmt<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub enum ModuleName<'cx> {
    Ident(&'cx Ident),
    StringLit(&'cx StringLit),
}

impl ModuleName<'_> {
    pub fn id(&self) -> NodeID {
        match self {
            ModuleName::Ident(ident) => ident.id,
            ModuleName::StringLit(string_lit) => string_lit.id,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            ModuleName::Ident(ident) => ident.span,
            ModuleName::StringLit(string_lit) => string_lit.span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Ident,
    pub ty_params: Option<TyParams<'cx>>,
    pub ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct InterfaceDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: &'cx Ident,
    pub ty_params: Option<TyParams<'cx>>,
    pub extends: Option<&'cx InterfaceExtendsClause<'cx>>,
    pub members: ObjectTyMembers<'cx>,
}

pub type ObjectTyMembers<'cx> = &'cx [&'cx ObjectTyMember<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct IndexSigDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectTyMember<'cx> {
    pub kind: ObjectTyMemberKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ObjectTyMemberKind<'cx> {
    IndexSig(&'cx IndexSigDecl<'cx>),
    Prop(&'cx PropSignature<'cx>),
    Method(&'cx MethodSignature<'cx>),
    CallSig(&'cx CallSigDecl<'cx>),
    CtorSig(&'cx CtorSigDecl<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct PropSignature<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: &'cx PropName<'cx>,
    pub question: Option<Span>,
    pub ty: Option<&'cx self::Ty<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct MethodSignature<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx PropName<'cx>,
    pub question: Option<Span>,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub stmts: self::Stmts<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ClassDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: &'cx Ident,
    pub ty_params: Option<TyParams<'cx>>,
    pub extends: Option<&'cx ClassExtendsClause<'cx>>,
    pub implements: Option<&'cx ClassImplementsClause<'cx>>,
    pub elems: &'cx ClassElems<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ClassElems<'cx> {
    pub span: Span,
    pub elems: &'cx [&'cx ClassElem<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct ClassElem<'cx> {
    pub kind: ClassEleKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ClassEleKind<'cx> {
    Ctor(&'cx ClassCtor<'cx>),
    Prop(&'cx ClassPropElem<'cx>),
    Method(&'cx ClassMethodElem<'cx>),
    IndexSig(&'cx IndexSigDecl<'cx>),
    Getter(&'cx GetterDecl<'cx>),
    Setter(&'cx SetterDecl<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct GetterDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: &'cx PropName<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
    pub body: Option<&'cx BlockStmt<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct SetterDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: &'cx PropName<'cx>,
    pub params: ParamsDecl<'cx>,
    pub body: Option<&'cx BlockStmt<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ClassCtor<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ret: Option<&'cx self::Ty<'cx>>,
    pub body: Option<&'cx BlockStmt<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ClassMethodElem<'cx> {
    pub id: NodeID,
    pub flags: NodeFlags,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: &'cx PropName<'cx>,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
    pub body: Option<&'cx BlockStmt<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ClassPropElem<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: &'cx PropName<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
    pub init: Option<&'cx Expr<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct TyParam<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Ident,
    pub constraint: Option<&'cx self::Ty<'cx>>,
    pub default: Option<&'cx self::Ty<'cx>>,
}

pub type TyParams<'cx> = &'cx [&'cx TyParam<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct InterfaceExtendsClause<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub list: &'cx [&'cx ReferTy<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct ClassExtendsClause<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx EntityName<'cx>,
    pub ty_args: Option<&'cx self::Tys<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ClassImplementsClause<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub list: &'cx [&'cx ReferTy<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct EmptyStmt {
    pub id: NodeID,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct RetStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: Option<&'cx Expr<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct IfStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
    pub then: &'cx Stmt<'cx>,
    pub else_then: Option<&'cx Stmt<'cx>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VarKind {
    Var,
    Let,
    Const,
}

pub type VarDecls<'cx> = &'cx [&'cx VarDecl<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct VarStmt<'cx> {
    pub id: NodeID,
    pub kind: VarKind,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub list: VarDecls<'cx>,
}

#[enumflags2::bitflags]
#[repr(u16)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ModifierKind {
    Public = 1 << 0,
    Private = 1 << 1,
    Protected = 1 << 2,
    Readonly = 1 << 3,
    Override = 1 << 4,
    Export = 1 << 5,
    Abstract = 1 << 6,
    Static = 1 << 7,
    Declare = 1 << 8,
}

impl std::fmt::Display for ModifierKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ModifierKind::Public => keyword::KW_PUBLIC_STR,
            ModifierKind::Private => keyword::KW_PRIVATE_STR,
            ModifierKind::Protected => keyword::KW_PROTECTED_STR,
            ModifierKind::Readonly => keyword::KW_READONLY_STR,
            ModifierKind::Override => "override",
            ModifierKind::Export => keyword::KW_EXPORT_STR,
            ModifierKind::Abstract => keyword::KW_ABSTRACT_STR,
            ModifierKind::Static => keyword::KW_STATIC_STR,
            ModifierKind::Declare => keyword::KW_DECLARE_STR,
        };
        write!(f, "{}", s)
    }
}

impl ModifierKind {
    pub const ACCESSIBILITY: enumflags2::BitFlags<ModifierKind> =
        enumflags2::make_bitflags!(ModifierKind::{Public | Private | Protected});
    pub const PARAMETER_PROPERTY: enumflags2::BitFlags<ModifierKind> =
        enumflags2::make_bitflags!(Self::{Public | Private | Protected | Readonly | Override});
}

#[derive(Debug, Clone, Copy)]
pub struct Modifiers<'cx> {
    pub span: Span,
    pub flags: enumflags2::BitFlags<ModifierKind>,
    pub list: &'cx [&'cx Modifier],
}

#[derive(Debug, Clone, Copy)]
pub struct Modifier {
    pub id: NodeID,
    pub span: Span,
    pub kind: ModifierKind,
}

#[derive(Debug, Clone, Copy)]
pub struct FnDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub flags: NodeFlags,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: &'cx Ident,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx super::Ty<'cx>>,
    pub body: Option<&'cx BlockStmt<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ParamDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub dotdotdot: Option<Span>,
    pub name: &'cx Ident,
    pub question: Option<Span>,
    pub ty: Option<&'cx self::Ty<'cx>>,
    pub init: Option<&'cx Expr<'cx>>,
}

pub type ParamsDecl<'cx> = &'cx [&'cx ParamDecl<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct ImportClause<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub is_type_only: bool,
    pub ident: Option<&'cx Ident>,
    pub kind: Option<ImportClauseKind<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub enum ImportClauseKind<'cx> {
    Ns(&'cx NsImport<'cx>),
    Specs(ImportSpecs<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct NsImport<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Ident,
}

#[derive(Debug, Clone, Copy)]
pub struct ImportSpec<'cx> {
    pub kind: ImportSpecKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ImportSpecKind<'cx> {
    Shorthand(&'cx ShorthandSpec<'cx>),
    Named(&'cx ImportNamedSpec<'cx>),
}

pub type ImportSpecs<'cx> = &'cx [&'cx ImportSpec<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct ShorthandSpec<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Ident,
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleExportName<'cx> {
    pub kind: ModuleExportNameKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ModuleExportNameKind<'cx> {
    Ident(&'cx Ident),
    StringLit(&'cx StringLit),
}

#[derive(Debug, Clone, Copy)]
pub struct ImportNamedSpec<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub prop_name: &'cx ModuleExportName<'cx>,
    pub name: &'cx Ident,
}

#[derive(Debug, Clone, Copy)]
pub struct ImportDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub clause: &'cx ImportClause<'cx>,
    pub module: &'cx StringLit,
}

#[derive(Debug, Clone, Copy)]
pub struct ExportDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub clause: &'cx ExportClause<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ExportClause<'cx> {
    pub is_type_only: bool,
    pub kind: ExportClauseKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ExportClauseKind<'cx> {
    Glob(&'cx GlobExport<'cx>),
    Ns(&'cx NsExport<'cx>),
    Specs(&'cx SpecsExport<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct GlobExport<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub module: &'cx StringLit,
}

#[derive(Debug, Clone, Copy)]
pub struct NsExport<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx ModuleExportName<'cx>,
    pub module: &'cx StringLit,
}

#[derive(Debug, Clone, Copy)]

pub struct SpecsExport<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub list: &'cx [&'cx ExportSpec<'cx>],
    pub module: Option<&'cx StringLit>,
}

#[derive(Debug, Clone, Copy)]
pub struct ExportSpec<'cx> {
    pub kind: ExportSpecKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ExportSpecKind<'cx> {
    Shorthand(&'cx ShorthandSpec<'cx>),
    Named(&'cx ExportNamedSpec<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct ExportNamedSpec<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub prop_name: &'cx ModuleExportName<'cx>,
    pub name: &'cx ModuleExportName<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum Binding<'cx> {
    Ident(&'cx Ident),
    ObjectPat(&'cx ObjectPat<'cx>),
    // ArrayPat()
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectPat<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub elems: ObjectBindingElems<'cx>,
}

pub type ObjectBindingElems<'cx> = &'cx [&'cx ObjectBindingElem<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct ObjectBindingElem<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub dotdotdot: Option<Span>,
    pub name: &'cx ObjectBindingName<'cx>,
    pub init: Option<&'cx Expr<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub enum ObjectBindingName<'cx> {
    Shorthand(&'cx Ident),
    Prop {
        prop_name: &'cx PropName<'cx>,
        name: &'cx Binding<'cx>,
    },
}
