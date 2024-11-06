mod node;
mod node_flags;

pub use node::{Node, NodeID};
use rts_span::Span;

use crate::atoms::AtomId;

#[derive(Debug, Clone, Copy)]
pub struct Program<'cx> {
    pub id: NodeID,
    pub stmts: Stmts<'cx>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Stmt<'cx> {
    pub id: NodeID,
    pub kind: StmtKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum StmtKind<'cx> {
    Empty(&'cx EmptyStmt),
    Var(&'cx VarStmt<'cx>),
    If(&'cx IfStmt<'cx>),
    Return(&'cx RetStmt<'cx>),
    Block(&'cx BlockStmt<'cx>),
    Fn(&'cx FnDecl<'cx>),
    Class(&'cx ClassDecl<'cx>),
    Expr(&'cx Expr<'cx>),
    Interface(&'cx InterfaceDecl<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct InterfaceDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Ident,
    pub members: ObjectTyMembers<'cx>,
}

pub type ObjectTyMembers<'cx> = &'cx [&'cx ObjectTyMember<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct ObjectTyMember<'cx> {
    pub kind: ObjectTyMemberKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ObjectTyMemberKind<'cx> {
    Prop(&'cx PropSignature<'cx>),
    Method(&'cx MethodSignature<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct PropSignature<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx PropName<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct MethodSignature<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx PropName<'cx>,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ret: Option<&'cx self::Ty<'cx>>,
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
    pub name: &'cx Ident,
    pub ty_params: Option<TyParams<'cx>>,
    pub extends: Option<&'cx HeritageClause<'cx>>,
    pub implements: Option<&'cx HeritageClauses<'cx>>,
    pub eles: ClassEles<'cx>,
}

pub type ClassEles<'cx> = &'cx [&'cx ClassEle<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct ClassEle<'cx> {
    pub kind: ClassEleKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ClassEleKind<'cx> {
    Prop(&'cx ClassPropEle<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct ClassPropEle<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx PropName<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
    pub init: Option<&'cx Expr<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct TyParam<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Ident,
}

pub type TyParams<'cx> = &'cx [&'cx TyParam<'cx>];
pub type Exprs<'cx> = &'cx [&'cx Expr<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct HeritageClauses<'cx> {
    pub span: Span,
    pub clauses: &'cx [&'cx HeritageClause<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct HeritageClause<'cx> {
    pub id: NodeID,
    pub span: Span,
    // FIXME: ExprWithArgs
    pub tys: Exprs<'cx>,
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

#[derive(Debug, Clone, Copy)]
pub struct VarStmt<'cx> {
    pub id: NodeID,
    pub kind: VarKind,
    pub span: Span,
    pub list: &'cx [&'cx VarDecl<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct Expr<'cx> {
    // pub id: NodeID,
    pub kind: ExprKind<'cx>,
}

impl Expr<'_> {
    pub fn span(&self) -> Span {
        use ExprKind::*;
        match self.kind {
            Bin(bin) => bin.span,
            BoolLit(lit) => lit.span,
            NumLit(lit) => lit.span,
            NullLit(lit) => lit.span,
            StringLit(lit) => lit.span,
            Ident(ident) => ident.span,
            ArrayLit(lit) => lit.span,
            Omit(expr) => expr.span,
            Paren(expr) => expr.span,
            Cond(cond) => cond.span,
            ObjectLit(lit) => lit.span,
            Call(call) => call.span,
            Fn(f) => f.span,
            New(new) => new.span,
            Assign(assign) => assign.span,
            ArrowFn(f) => f.span
        }
    }

    pub fn id(&self) -> NodeID {
        use ExprKind::*;
        match self.kind {
            Bin(bin) => bin.id,
            BoolLit(lit) => lit.id,
            NumLit(lit) => lit.id,
            NullLit(lit) => lit.id,
            StringLit(lit) => lit.id,
            Ident(ident) => ident.id,
            ArrayLit(lit) => lit.id,
            Omit(expr) => expr.id,
            Paren(expr) => expr.id,
            Cond(cond) => cond.id,
            ObjectLit(lit) => lit.id,
            Call(call) => call.id,
            Fn(f) => f.id,
            New(new) => new.id,
            Assign(assign) => assign.id,
            ArrowFn(f) => f.id
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'cx> {
    Assign(&'cx AssignExpr<'cx>),
    Bin(&'cx BinExpr<'cx>),
    BoolLit(&'cx BoolLit),
    NumLit(&'cx NumLit),
    StringLit(&'cx StringLit),
    NullLit(&'cx NullLit),
    ArrayLit(&'cx ArrayLit<'cx>),
    Ident(&'cx Ident),
    Omit(&'cx OmitExpr),
    Paren(&'cx ParenExpr<'cx>),
    Cond(&'cx CondExpr<'cx>),
    ObjectLit(&'cx ObjectLit<'cx>),
    Call(&'cx CallExpr<'cx>),
    Fn(&'cx FnExpr<'cx>),
    New(&'cx NewExpr<'cx>),
    ArrowFn(&'cx ArrowFnExpr<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct ArrowFnExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
    pub body: &'cx BlockStmt<'cx>
}

#[derive(Debug, Clone, Copy)]
pub enum AssignOp {
    Eq,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    ShlEq,
    ShrEq,
    UShrEq,
    BitAndEq,
    BitXorEq,
    BitOrEq,
}

impl AssignOp {
    pub fn as_str(self) -> &'static str {
        use AssignOp::*;
        match self {
            Eq => "=",
            AddEq => "+=",
            SubEq => "-=",
            MulEq => "*=",
            DivEq => "/=",
            ModEq => "%=",
            ShlEq => "<<=",
            ShrEq => ">>=",
            UShrEq => ">>>=",
            BitAndEq => "&=",
            BitXorEq => "^=",
            BitOrEq => "|=",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AssignExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub binding: &'cx Ident,
    pub op: AssignOp,
    pub right: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct NewExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
    pub args: Option<Exprs<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct FnExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: Option<&'cx Ident>,
    pub params: ParamsDecl<'cx>,
    pub ret_ty: Option<&'cx self::Ty<'cx>>,
    pub body: &'cx BlockStmt<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct CondExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub cond: &'cx Expr<'cx>,
    pub when_true: &'cx Expr<'cx>,
    pub when_false: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ParenExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ArrayLit<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub elems: Exprs<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HeritageClauseKind {
    Extends,
    Implements,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Pipe,
    PipePipe,
    Less,
    LessEq,
    Shl,
    Great,
    GreatEq,
    Shr,
    UShr,
    BitAnd,
    LogicalAnd,
    EqEq,
    EqEqEq,
}

impl BinOpKind {
    pub fn as_str(self) -> &'static str {
        use BinOpKind::*;
        match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Pipe => "|",
            PipePipe => "||",
            Less => "<",
            LessEq => "<=",
            Shl => "<<",
            Great => ">",
            GreatEq => ">=",
            Shr => ">>",
            UShr => ">>>",
            BitAnd => "&",
            LogicalAnd => "&&",
            EqEq => "==",
            EqEqEq => "===",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BinExpr<'cx> {
    pub id: NodeID,
    pub left: &'cx Expr<'cx>,
    pub op: BinOp,
    pub right: &'cx Expr<'cx>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Lit<T> {
    pub id: NodeID,
    pub val: T,
    pub span: Span,
}

pub type NumLit = Lit<f64>;
pub type BoolLit = Lit<bool>;
pub type NullLit = Lit<()>;
pub type StringLit = Lit<AtomId>;

#[derive(Debug, Clone, Copy)]
pub struct VarDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub binding: &'cx Ident,
    pub ty: Option<&'cx self::Ty<'cx>>,
    pub init: Option<&'cx Expr<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Ident {
    pub id: NodeID,
    pub span: Span,
    pub name: AtomId,
}

#[derive(Debug, Clone, Copy)]
pub struct OmitExpr {
    pub id: NodeID,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Ty<'cx> {
    pub kind: TyKind<'cx>,
}

impl Ty<'_> {
    pub fn span(&self) -> Span {
        match self.kind {
            TyKind::Ident(ident) => ident.span,
            TyKind::Array(array) => array.span,
            TyKind::Fn(f) => f.span,
            TyKind::Lit(lit) => lit.span,
        }
    }

    pub fn id(&self) -> NodeID {
        match self.kind {
            TyKind::Ident(node) => node.id,
            TyKind::Array(node) => node.id,
            TyKind::Fn(node) => node.id,
            TyKind::Lit(node) => node.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TyKind<'cx> {
    Ident(&'cx Ident),
    Array(&'cx ArrayTy<'cx>),
    Fn(&'cx FnTy<'cx>),
    Lit(&'cx LitTy<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct LitTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub members: ObjectTyMembers<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct FnTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub params: ParamsDecl<'cx>,
    pub ret_ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ArrayTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ele: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct PropName<'cx> {
    pub kind: PropNameKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum PropNameKind<'cx> {
    Ident(&'cx Ident),
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectMemberField<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx PropName<'cx>,
    pub value: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectLit<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub members: &'cx [&'cx ObjectMemberField<'cx>],
}

pub type ParamsDecl<'cx> = &'cx [&'cx ParamDecl<'cx>];
pub type Stmts<'cx> = &'cx [&'cx Stmt<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct FnDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Ident,
    pub params: ParamsDecl<'cx>,
    pub ret_ty: Option<&'cx self::Ty<'cx>>,
    pub body: &'cx BlockStmt<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ParamDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub dotdotdot: Option<Span>,
    pub name: &'cx Ident,
    pub question: Option<Span>,
    pub ty: Option<&'cx self::Ty<'cx>>,
    pub init: Option<&'cx Expr<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CallExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
    pub args: Exprs<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ModifierKind {
    Public,
    Abstract
}

#[derive(Debug, Clone, Copy)]
pub struct Modifiers<'cx> {
    pub span: Span,
    pub list: &'cx [&'cx Modifier],
}

#[derive(Debug, Clone, Copy)]
pub struct Modifier {
    pub id: NodeID,
    pub span: Span,
    pub kind: ModifierKind,
}
