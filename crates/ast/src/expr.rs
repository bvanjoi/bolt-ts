use bolt_ts_atom::AtomId;

use super::*;

pub type Exprs<'cx> = &'cx [&'cx Expr<'cx>];

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
            PostfixUnary(unary) => unary.span,
            PropAccess(a) => a.span,
            EleAccess(a) => a.span,
            This(this) => this.span,
            Typeof(n) => n.span,
            Super(n) => n.span,
            Void(n) => n.span,
            As(n) => n.span,
            Satisfies(n) => n.span,
            NonNull(n) => n.span,
            Template(n) => n.span,
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
            PostfixUnary(unary) => unary.id,
            PropAccess(a) => a.id,
            EleAccess(a) => a.id,
            This(this) => this.id,
            Typeof(n) => n.id,
            Super(n) => n.id,
            Void(n) => n.id,
            As(n) => n.id,
            Satisfies(n) => n.id,
            NonNull(n) => n.id,
            Template(n) => n.id,
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
    PostfixUnary(&'cx PostfixUnaryExpr<'cx>),
    PropAccess(&'cx PropAccessExpr<'cx>),
    EleAccess(&'cx EleAccessExpr<'cx>),
    Super(&'cx SuperExpr),
    Typeof(&'cx TypeofExpr<'cx>),
    Void(&'cx VoidExpr<'cx>),
    As(&'cx AsExpr<'cx>),
    Satisfies(&'cx SatisfiesExpr<'cx>),
    NonNull(&'cx NonNullExpr<'cx>),
    Template(&'cx TemplateExpr<'cx>),
}

impl<'cx> ExprKind<'cx> {
    fn skip_paren(&'cx self) -> &'cx ExprKind<'cx> {
        match self {
            ExprKind::Paren(p) => p.expr.kind.skip_paren(),
            _ => self,
        }
    }

    pub fn is_logical_assignment(&self) -> bool {
        let expr = self.skip_paren();
        expr.is_logical_or_coalescing_assignment()
    }

    fn is_logical_or_coalescing_assignment(&self) -> bool {
        match self {
            ExprKind::Assign(n) => n.op.is_logical_or_coalescing_assign_op(),
            _ => false,
        }
    }

    fn is_logical_or_coalescing_binary(&self) -> bool {
        match self {
            ExprKind::Bin(bin) => bin.op.kind.is_logical_or_coalescing_op(),
            _ => false,
        }
    }

    pub fn is_logical_expr(&self) -> bool {
        let mut n = self;
        loop {
            match n {
                ExprKind::Paren(p) => n = &p.expr.kind,
                ExprKind::PrefixUnary(p) if p.op == PrefixUnaryOp::Excl => n = &p.expr.kind,
                _ => return self.is_logical_or_coalescing_binary(),
            };
        }
    }

    pub fn is_type_assertion(&self) -> bool {
        match self {
            ExprKind::Paren(p) => p.expr.kind.is_type_assertion(),
            ExprKind::As(_) => true,
            // TODO: type assertion expression, is jsdoc type assertion
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TemplateExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub head: &'cx TemplateHead,
    pub spans: &'cx [&'cx TemplateSpan<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct TemplateHead {
    pub id: NodeID,
    pub span: Span,
    pub text: AtomId,
}

#[derive(Debug, Clone, Copy)]
pub struct TemplateSpan<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx self::Expr<'cx>,
    pub text: AtomId,
    pub is_tail: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct NonNullExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct SatisfiesExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
    pub ty: &'cx Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct AsExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
    pub ty: &'cx Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct VoidExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct SuperExpr {
    pub id: NodeID,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeofExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrefixUnaryOp {
    Plus,
    Minus,
    PlusPlus,
    MinusMinus,
    Tilde,
    Excl,
}

impl PrefixUnaryOp {
    pub fn as_str(self) -> &'static str {
        use PrefixUnaryOp::*;
        match self {
            Plus => "+",
            Minus => "-",
            PlusPlus => "++",
            MinusMinus => "--",
            Tilde => "~",
            Excl => "!",
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
pub enum PostfixUnaryOp {
    PlusPlus,
    MinusMinus,
}

impl PostfixUnaryOp {
    pub fn as_str(self) -> &'static str {
        use PostfixUnaryOp::*;
        match self {
            PlusPlus => "++",
            MinusMinus => "--",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PostfixUnaryExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub op: PostfixUnaryOp,
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

#[derive(Debug, Clone, Copy, PartialEq)]
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

    pub fn is_logical_or_coalescing_assign_op(self) -> bool {
        // ||=, ??=
        false
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
    pub ty_args: Option<&'cx self::Tys<'cx>>,
    pub args: Option<Exprs<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ClassExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: Option<&'cx Ident>,
    pub ty_params: Option<TyParams<'cx>>,
    pub extends: Option<&'cx ClassExtendsClause<'cx>>,
    pub implements: Option<&'cx ClassImplementsClause<'cx>>,
    pub elems: &'cx ClassElems<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct FnExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: Option<&'cx Ident>,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
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
    NEq,
    NEqEq,
    Instanceof,
    In,
    Satisfies,
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
            Instanceof => "instanceof",
            In => "in",
            Satisfies => "satisfies",
            NEq => "!=",
            NEqEq => "!==",
        }
    }

    fn is_logical_op(self) -> bool {
        // TODO:  AmpersandAmpersand
        matches!(self, Self::PipePipe)
    }

    pub fn is_logical_or_coalescing_op(self) -> bool {
        // TODO: QuestionQuestion
        self.is_logical_op() || false
    }
}

impl std::fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
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
    pub binding: &'cx Binding<'cx>,
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
pub struct CallExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub flags: NodeFlags,
    pub expr: &'cx Expr<'cx>,
    pub ty_args: Option<&'cx self::Tys<'cx>>,
    pub args: Exprs<'cx>,
}

impl CallExpr<'_> {
    pub fn is_call_chain(&self) -> bool {
        self.flags.intersects(NodeFlags::OPTIONAL_CHAIN)
    }
}
