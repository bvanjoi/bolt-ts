mod node;
mod node_flags;

use bolt_ts_span::Span;
pub use node::{Node, NodeID};

use crate::atoms::AtomId;

#[derive(Debug, Clone, Copy)]
pub struct Program<'cx> {
    pub id: NodeID,
    pub stmts: Stmts<'cx>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Stmt<'cx> {
    // pub id: NodeID,
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
    Type(&'cx TypeDecl<'cx>),
    Namespace(&'cx NsDecl<'cx>),
    Throw(&'cx ThrowStmt<'cx>),
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
    pub name: &'cx Ident,
    pub block: &'cx BlockStmt<'cx>,
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
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: &'cx Ident,
    pub ty_params: Option<TyParams<'cx>>,
    pub extends: Option<&'cx ClassExtendsClause<'cx>>,
    pub implements: Option<&'cx ImplementsClause<'cx>>,
    pub elems: &'cx ClassElems<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ClassElems<'cx> {
    pub span: Span,
    pub elems: &'cx [&'cx ClassEle<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct ClassEle<'cx> {
    pub kind: ClassEleKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ClassEleKind<'cx> {
    Ctor(&'cx ClassCtor<'cx>),
    Prop(&'cx ClassPropEle<'cx>),
    Method(&'cx ClassMethodEle<'cx>),
    IndexSig(&'cx IndexSigDecl<'cx>),
    Getter(&'cx GetterDecl<'cx>),
    Setter(&'cx SetterDecl<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct GetterDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx PropName<'cx>,
    pub ret: Option<&'cx self::Ty<'cx>>,
    pub body: Option<&'cx BlockStmt<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct SetterDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
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
pub struct ClassMethodEle<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: &'cx PropName<'cx>,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ret: Option<&'cx self::Ty<'cx>>,
    pub body: Option<&'cx BlockStmt<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ClassPropEle<'cx> {
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
pub type Exprs<'cx> = &'cx [&'cx Expr<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct InterfaceExtendsClause<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tys: Tys<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ClassExtendsClause<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ImplementsClause<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tys: Tys<'cx>,
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
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub list: &'cx [&'cx VarDecl<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct Expr<'cx> {
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
            Class(c) => c.span,
            New(new) => new.span,
            Assign(assign) => assign.span,
            ArrowFn(f) => f.span,
            PrefixUnary(unary) => unary.span,
            PropAccess(a) => a.span,
            EleAccess(a) => a.span,
            This(this) => this.span,
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
            Class(c) => c.id,
            New(new) => new.id,
            Assign(assign) => assign.id,
            ArrowFn(f) => f.id,
            PrefixUnary(unary) => unary.id,
            PropAccess(a) => a.id,
            EleAccess(a) => a.id,
            This(this) => this.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'cx> {
    Assign(&'cx AssignExpr<'cx>),
    Bin(&'cx BinExpr<'cx>),
    This(&'cx ThisExpr),
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
    Class(&'cx ClassExpr<'cx>),
    New(&'cx NewExpr<'cx>),
    ArrowFn(&'cx ArrowFnExpr<'cx>),
    PrefixUnary(&'cx PrefixUnaryExpr<'cx>),
    PropAccess(&'cx PropAccessExpr<'cx>),
    EleAccess(&'cx EleAccessExpr<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct ThisExpr {
    pub id: NodeID,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct EleAccessExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
    pub arg: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct PropAccessExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
    pub name: &'cx Ident,
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixUnaryOp {
    Plus,
    Minus,
    PlusPlus,
    MinusMinus,
}

impl PrefixUnaryOp {
    pub fn as_str(self) -> &'static str {
        use PrefixUnaryOp::*;
        match self {
            Plus => "+",
            Minus => "-",
            PlusPlus => "++",
            MinusMinus => "--",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PrefixUnaryExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub op: PrefixUnaryOp,
    pub expr: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ArrowFnExprBody<'cx> {
    Block(&'cx BlockStmt<'cx>),
    Expr(&'cx Expr<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct ArrowFnExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
    pub body: ArrowFnExprBody<'cx>,
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
    pub left: &'cx Expr<'cx>,
    pub op: AssignOp,
    pub right: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct NewExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
    pub ty_args: Option<self::Tys<'cx>>,
    pub args: Option<Exprs<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ClassExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: Option<&'cx Ident>,
    pub ty_params: Option<TyParams<'cx>>,
    pub extends: Option<&'cx ClassExtendsClause<'cx>>,
    pub implements: Option<&'cx ImplementsClause<'cx>>,
    pub elems: &'cx ClassElems<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct FnExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: Option<&'cx Ident>,
    pub ty_params: Option<TyParams<'cx>>,
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

pub type Tys<'cx> = &'cx [&'cx Ty<'cx>];

impl Ty<'_> {
    pub fn span(&self) -> Span {
        match self.kind {
            TyKind::Array(array) => array.span,
            TyKind::Fn(f) => f.span,
            TyKind::ObjectLit(lit) => lit.span,
            TyKind::ExprWithArg(node) => node.span(),
            TyKind::NumLit(num) => num.span,
            TyKind::StringLit(s) => s.span,
            TyKind::Tuple(tuple) => tuple.span,
            TyKind::Rest(rest) => rest.span,
            TyKind::IndexedAccess(n) => n.span,
            TyKind::Cond(n) => n.span,
            TyKind::Refer(n) => n.span,
            TyKind::Union(n) => n.span,
            TyKind::Intersection(n) => n.span,
            TyKind::BooleanLit(n) => n.span,
            TyKind::NullLit(n) => n.span,
        }
    }

    pub fn id(&self) -> NodeID {
        match self.kind {
            TyKind::Array(node) => node.id,
            TyKind::Fn(node) => node.id,
            TyKind::ObjectLit(node) => node.id,
            TyKind::ExprWithArg(node) => node.id(),
            TyKind::NumLit(num) => num.id,
            TyKind::StringLit(s) => s.id,
            TyKind::Tuple(tuple) => tuple.id,
            TyKind::Rest(rest) => rest.id,
            TyKind::IndexedAccess(n) => n.id,
            TyKind::Cond(n) => n.id,
            TyKind::Refer(n) => n.id,
            TyKind::Union(n) => n.id,
            TyKind::Intersection(n) => n.id,
            TyKind::BooleanLit(n) => n.id,
            TyKind::NullLit(n) => n.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TyKind<'cx> {
    Refer(&'cx ReferTy<'cx>),
    Array(&'cx ArrayTy<'cx>),
    IndexedAccess(&'cx IndexedAccessTy<'cx>),
    Fn(&'cx FnTy<'cx>),
    ObjectLit(&'cx ObjectLitTy<'cx>),
    ExprWithArg(&'cx Expr<'cx>),
    NumLit(&'cx NumLitTy),
    BooleanLit(&'cx BoolLitTy),
    NullLit(&'cx NullLitTy),
    StringLit(&'cx StringLitTy),
    Tuple(&'cx TupleTy<'cx>),
    Rest(&'cx RestTy<'cx>),
    Cond(&'cx CondTy<'cx>),
    Union(&'cx UnionTy<'cx>),
    Intersection(&'cx IntersectionTy<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct LitTy<T> {
    pub id: NodeID,
    pub val: T,
    pub span: Span,
}

pub type NumLitTy = LitTy<f64>;
pub type BoolLitTy = LitTy<bool>;
pub type NullLitTy = LitTy<()>;
pub type StringLitTy = LitTy<AtomId>;

#[derive(Debug, Clone, Copy)]
pub struct UnionTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tys: Tys<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct IntersectionTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tys: Tys<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ReferTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx EntityName<'cx>,
    pub ty_args: Option<Tys<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CondTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub check_ty: &'cx self::Ty<'cx>,
    pub extends_ty: &'cx self::Ty<'cx>,
    pub true_ty: &'cx self::Ty<'cx>,
    pub false_ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct RestTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct TupleTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tys: Tys<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct CtorSigDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CallSigDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectLitTy<'cx> {
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
pub struct IndexedAccessTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx self::Ty<'cx>,
    pub index_ty: &'cx self::Ty<'cx>,
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

impl<'cx> PropName<'cx> {
    pub fn span(&self) -> Span {
        match self.kind {
            PropNameKind::Ident(ident) => ident.span,
            PropNameKind::NumLit(num) => num.span,
            PropNameKind::StringLit(lit) => lit.span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PropNameKind<'cx> {
    Ident(&'cx Ident),
    StringLit(&'cx StringLit),
    NumLit(&'cx NumLit),
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
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub name: &'cx Ident,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ret_ty: Option<&'cx self::Ty<'cx>>,
    pub body: Option<&'cx BlockStmt<'cx>>,
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
    pub ty_args: Option<self::Tys<'cx>>,
    pub args: Exprs<'cx>,
}

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ModifierKind {
    Public = 1 << 0,
    Private = 1 << 1,
    Abstract = 1 << 2,
    Static = 1 << 3,
    Declare = 1 << 4,
    Export = 1 << 5,
    Readonly = 1 << 6,
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
pub struct EntityName<'cx> {
    pub kind: EntityNameKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum EntityNameKind<'cx> {
    Ident(&'cx Ident),
    Qualified(&'cx QualifiedName<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct QualifiedName<'cx> {
    pub id: NodeID,
    pub left: &'cx EntityName<'cx>,
    pub right: &'cx Ident,
}
