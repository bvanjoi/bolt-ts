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
    Expr(&'cx ExprStmt<'cx>),
    Interface(&'cx InterfaceDecl<'cx>),
    Type(&'cx TypeDecl<'cx>),
    Namespace(&'cx NsDecl<'cx>),
    Throw(&'cx ThrowStmt<'cx>),
    Enum(&'cx EnumDecl<'cx>),
    Import(&'cx ImportDecl<'cx>),
    Export(&'cx ExportDecl<'cx>),
    ExportAssign(&'cx ExportAssign<'cx>),
    Try(&'cx TryStmt<'cx>),
    While(&'cx WhileStmt<'cx>),
    Do(&'cx DoStmt<'cx>),
    Debugger(&'cx DebuggerStmt),
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
            Expr(expr) => expr.id,
            Interface(i) => i.id,
            Type(t) => t.id,
            Namespace(n) => n.id,
            Throw(t) => t.id,
            Enum(e) => e.id,
            Import(n) => n.id,
            Export(n) => n.id,
            ExportAssign(n) => n.id,
            For(n) => n.id,
            ForOf(n) => n.id,
            ForIn(n) => n.id,
            Break(n) => n.id,
            Continue(n) => n.id,
            Try(n) => n.id,
            While(n) => n.id,
            Do(n) => n.id,
            Debugger(n) => n.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ExprStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx super::Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct DebuggerStmt {
    pub id: NodeID,
    pub span: Span,
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
    Var(VarDecls<'cx>),
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
    pub is_global_argument: bool,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: ModuleName<'cx>,
    pub block: Option<&'cx ModuleBlock<'cx>>,
}

impl NsDecl<'_> {
    pub fn is_ambient(&self) -> bool {
        matches!(self.name, ModuleName::StringLit(_)) || self.is_global_scope_argument()
    }

    pub fn is_global_scope_argument(&self) -> bool {
        self.is_global_argument
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleBlock<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub stmts: self::Stmts<'cx>,
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
    pub modifiers: Option<&'cx Modifiers<'cx>>,
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
    Setter(&'cx SetterDecl<'cx>),
    Getter(&'cx GetterDecl<'cx>),
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
    /// `None` for export decl, eg: `export default class {}`
    pub name: Option<&'cx Ident>,
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

impl ClassEleKind<'_> {
    pub fn is_static(&self) -> bool {
        use ClassEleKind::*;
        let ms = match self {
            Ctor(_) => None,
            Prop(n) => n.modifiers,
            Method(n) => n.modifiers,
            IndexSig(n) => n.modifiers,
            Getter(n) => n.modifiers,
            Setter(n) => n.modifiers,
        };
        ms.is_some_and(|ms| ms.flags.contains(ModifierKind::Static))
    }

    pub fn id(&self) -> NodeID {
        use ClassEleKind::*;
        match self {
            Ctor(n) => n.id,
            Prop(n) => n.id,
            Method(n) => n.id,
            IndexSig(n) => n.id,
            Getter(n) => n.id,
            Setter(n) => n.id,
        }
    }
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

impl<'cx> SetterDecl<'cx> {
    pub fn get_effective_ty_annotation_node(&self) -> Option<&'cx self::Ty<'cx>> {
        self.get_value_param().and_then(|p| p.ty)
    }

    pub fn get_value_param(&self) -> Option<&'cx self::ParamDecl<'cx>> {
        (!self.params.is_empty()).then(|| {
            let len = self.params.len();
            if len == 2 {
                if let self::BindingKind::Ident(i) = self.params[0].name.kind {
                    if i.name == keyword::KW_THIS {
                        return self.params[1];
                    }
                }
            }
            self.params[0]
        })
    }
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
    pub question: Option<Span>,
    pub excl: Option<Span>,
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

pub type VarDecls<'cx> = &'cx [&'cx VarDecl<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct VarStmt<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub list: VarDecls<'cx>,
}

#[enumflags2::bitflags]
#[repr(u32)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ModifierKind {
    Public = 1 << 0,
    Private = 1 << 1,
    Protected = 1 << 2,
    Readonly = 1 << 3,
    Override = 1 << 4,
    Export = 1 << 5,
    Abstract = 1 << 6,
    Ambient = 1 << 7,
    Static = 1 << 8,
    Accessor = 1 << 9,
    Async = 1 << 10,
    Default = 1 << 11,
    Const = 1 << 12,
    In = 1 << 13,
    Out = 1 << 14,
    Decorator = 1 << 15,
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
            ModifierKind::Ambient => keyword::KW_DECLARE_STR,
            ModifierKind::Default => keyword::KW_DEFAULT_STR,
            ModifierKind::Decorator => todo!(),
            ModifierKind::Accessor => todo!(),
            ModifierKind::Async => todo!(),
            ModifierKind::Const => todo!(),
            ModifierKind::In => todo!(),
            ModifierKind::Out => todo!(),
        };
        write!(f, "{}", s)
    }
}

impl ModifierKind {
    pub const SYNTACTIC_OR_JS_MODIFIERS: enumflags2::BitFlags<ModifierKind> = enumflags2::make_bitflags!(ModifierKind::{Public | Private | Protected | Readonly | Override});
    pub const SYNTACTIC_ONLY_MODIFIERS: enumflags2::BitFlags<ModifierKind> = enumflags2::make_bitflags!(ModifierKind::{Export | Ambient | Abstract | Static | Accessor | Async | Default | Const | In | Out | Decorator});
    // pub const SYNTACTIC_MODIFIER: enumflags2::BitFlags<ModifierKind> =
    //     enumflags2::BitFlags::<ModifierKind>::from_bits_truncate_c(
    //         Self::SYNTACTIC_OR_JS_MODIFIERS.bits() | Self::SYNTACTIC_ONLY_MODIFIERS.bits(),
    //         enumflags2::BitFlags::CONST_TOKEN,
    //     );

    pub const ACCESSIBILITY: enumflags2::BitFlags<ModifierKind> =
        enumflags2::make_bitflags!(ModifierKind::{Public | Private | Protected});
    pub const PARAMETER_PROPERTY: enumflags2::BitFlags<ModifierKind> =
        enumflags2::make_bitflags!(Self::{Public | Private | Protected | Readonly | Override});
    pub const NON_PUBLIC_ACCESSIBILITY_MODIFIER: enumflags2::BitFlags<ModifierKind> =
        enumflags2::make_bitflags!(ModifierKind::{Private | Protected});

    fn syntactic_modifier() -> enumflags2::BitFlags<ModifierKind> {
        Self::SYNTACTIC_ONLY_MODIFIERS | Self::SYNTACTIC_OR_JS_MODIFIERS
    }

    pub fn get_syntactic_modifier_flags(
        flags: enumflags2::BitFlags<ModifierKind>,
    ) -> enumflags2::BitFlags<ModifierKind> {
        flags & Self::syntactic_modifier()
    }
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
    pub name: &'cx Binding<'cx>,
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
    pub name: Option<&'cx Ident>,
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

impl ModuleExportName<'_> {
    pub fn is_default(&self) -> bool {
        let v = match self.kind {
            ModuleExportNameKind::Ident(ident) => ident.name,
            ModuleExportNameKind::StringLit(lit) => lit.val,
        };
        v == keyword::KW_DEFAULT
    }

    pub fn span(&self) -> Span {
        use ModuleExportNameKind::*;
        match self.kind {
            Ident(ident) => ident.span,
            StringLit(lit) => lit.span,
        }
    }
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

impl<'cx> ExportDecl<'cx> {
    pub fn module_spec(&self) -> Option<&'cx StringLit> {
        match self.clause.kind {
            ExportClauseKind::Glob(g) => Some(g.module),
            ExportClauseKind::Ns(n) => Some(n.module),
            ExportClauseKind::Specs(s) => s.module,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ExportAssign<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub expr: &'cx Expr<'cx>,
    pub is_export_equals: bool,
}

impl ExportAssign<'_> {
    pub fn is_aliasable(&self) -> bool {
        self.expr.is_entity_name_expr() || matches!(self.expr.kind, ExprKind::Class(_))
    }
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

impl ExportSpec<'_> {
    pub fn id(&self) -> NodeID {
        use ExportSpecKind::*;
        match self.kind {
            Shorthand(shorthand) => shorthand.id,
            Named(named) => named.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExportSpecKind<'cx> {
    Shorthand(&'cx ShorthandSpec<'cx>),
    Named(&'cx ExportNamedSpec<'cx>),
}

/// ```txt
/// export { prop_name as name } from 'xxx'
///          ^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, Clone, Copy)]
pub struct ExportNamedSpec<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub prop_name: &'cx ModuleExportName<'cx>,
    pub name: &'cx ModuleExportName<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct Binding<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub kind: BindingKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum BindingKind<'cx> {
    Ident(&'cx Ident),
    ObjectPat(&'cx ObjectPat<'cx>),
    ArrayPat(&'cx ArrayPat),
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectPat<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub elems: ObjectBindingElems<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ArrayPat {
    pub id: NodeID,
    pub span: Span,
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
