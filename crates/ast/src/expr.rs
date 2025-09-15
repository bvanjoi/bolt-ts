use bolt_ts_atom::Atom;
use bolt_ts_ecma_logical::js_double_to_boolean;

use super::*;

pub type Exprs<'cx> = &'cx [&'cx Expr<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct Expr<'cx> {
    pub kind: ExprKind<'cx>,
}

impl<'cx> Expr<'cx> {
    pub fn span(&self) -> Span {
        use ExprKind::*;
        match self.kind {
            Bin(bin) => bin.span,
            BoolLit(lit) => lit.span,
            NumLit(lit) => lit.span,
            NullLit(lit) => lit.span,
            BigIntLit(lit) => lit.span,
            StringLit(lit) => lit.span,
            NoSubstitutionTemplateLit(n) => n.span,
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
            TyAssertion(n) => n.span,
            ExprWithTyArgs(n) => n.span,
            SpreadElement(n) => n.span,
            RegExpLit(n) => n.span,
            TaggedTemplate(n) => n.span,
            JsxElem(n) => n.span,
            JsxSelfClosingElem(n) => n.span,
            JsxFrag(n) => n.span,
        }
    }

    pub fn id(&self) -> NodeID {
        use ExprKind::*;
        match self.kind {
            Bin(bin) => bin.id,
            BoolLit(lit) => lit.id,
            NumLit(lit) => lit.id,
            BigIntLit(lit) => lit.id,
            NullLit(lit) => lit.id,
            StringLit(lit) => lit.id,
            NoSubstitutionTemplateLit(lit) => lit.id,
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
            TyAssertion(n) => n.id,
            ExprWithTyArgs(n) => n.id,
            SpreadElement(n) => n.id,
            RegExpLit(n) => n.id,
            TaggedTemplate(n) => n.id,
            JsxElem(n) => n.id,
            JsxSelfClosingElem(n) => n.id,
            JsxFrag(n) => n.id,
        }
    }

    pub fn is_string_lit_like(&self) -> bool {
        matches!(
            self.kind,
            ExprKind::StringLit(_) | ExprKind::NoSubstitutionTemplateLit(_)
        )
    }

    pub fn is_string_or_number_lit_like(&self) -> bool {
        self.is_string_lit_like() || matches!(self.kind, ExprKind::NumLit(_))
    }

    pub fn is_entity_name_expr(&self) -> bool {
        matches!(self.kind, ExprKind::Ident(_)) || self.is_prop_access_entity_name_expr()
    }

    pub fn is_prop_access_entity_name_expr(&self) -> bool {
        if let ExprKind::PropAccess(p) = &self.kind {
            // TODO: matches!(p.name.kind, ExprKind::Ident(_))
            p.expr.is_entity_name_expr()
        } else {
            false
        }
    }

    fn is_outer_expr(&self) -> bool {
        match self.kind {
            ExprKind::Paren(_) => true,
            // TODO: handle more case
            _ => false,
        }
    }

    pub fn skip_outer_expr(mut expr: &'cx Expr<'cx>) -> &'cx Expr<'cx> {
        while expr.is_outer_expr() {
            if let ExprKind::Paren(child) = expr.kind {
                expr = child.expr;
            }
        }
        expr
    }

    pub fn skip_parens(expr: &'cx Expr<'cx>) -> &'cx Expr<'cx> {
        Self::skip_outer_expr(expr)
    }

    pub fn is_super_prop(&self) -> bool {
        match self.kind {
            ExprKind::PropAccess(p) => matches!(p.expr.kind, ExprKind::Super(_)),
            ExprKind::EleAccess(e) => matches!(e.expr.kind, ExprKind::Super(_)),
            _ => false,
        }
    }

    pub fn is_dotted_name(&self) -> bool {
        use self::ExprKind::*;
        match self.kind {
            Ident(_) | This(_) | Super(_) => true,
            PropAccess(n) => n.expr.is_dotted_name(),
            Paren(n) => n.expr.is_dotted_name(),
            _ => false,
        }
    }

    pub fn is_left_hand_side_expr_kind(&self) -> bool {
        use self::ExprKind::*;
        matches!(
            self.kind,
            PropAccess(_)
                | EleAccess(_)
                | New(_)
                | Call(_)
                | ArrayLit(_)
                | Paren(_)
                | ObjectLit(_)
                | Class(_)
                | Fn(_)
                | Ident(_)
                | NullLit(_)
                | This(_)
                | NumLit(_)
                | StringLit(_)
                | BoolLit(_)
                | Template(_)
                | Super(_)
                | NonNull(_)
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'cx> {
    Assign(&'cx AssignExpr<'cx>),
    Bin(&'cx BinExpr<'cx>),
    This(&'cx ThisExpr),
    BoolLit(&'cx BoolLit),
    NumLit(&'cx NumLit),
    BigIntLit(&'cx BigIntLit),
    StringLit(&'cx StringLit),
    NoSubstitutionTemplateLit(&'cx NoSubstitutionTemplateLit),
    NullLit(&'cx NullLit),
    RegExpLit(&'cx RegExpLit),
    ArrayLit(&'cx ArrayLit<'cx>),
    Ident(&'cx Ident),
    Omit(&'cx OmitExpr),
    Paren(&'cx ParenExpr<'cx>),
    Cond(&'cx CondExpr<'cx>),
    ObjectLit(&'cx ObjectLit<'cx>),
    ExprWithTyArgs(&'cx ExprWithTyArgs<'cx>),
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
    TaggedTemplate(&'cx TaggedTemplateExpr<'cx>),
    TyAssertion(&'cx TyAssertion<'cx>),
    SpreadElement(&'cx SpreadElement<'cx>),
    JsxElem(&'cx JsxElem<'cx>),
    JsxSelfClosingElem(&'cx JsxSelfClosingElem<'cx>),
    JsxFrag(&'cx JsxFrag<'cx>),
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

    pub fn as_literal_to_boolean(&self) -> Option<bool> {
        match self {
            ExprKind::BoolLit(lit) => Some(lit.val),
            ExprKind::NumLit(lit) => Some(js_double_to_boolean(lit.val)),
            ExprKind::StringLit(lit) => Some(lit.val != keyword::IDENT_EMPTY),
            ExprKind::NullLit(_) => Some(false),
            // TODO: bigint and undefined
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SpreadElement<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ExprWithTyArgs<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
    pub ty_args: Option<&'cx self::Tys<'cx>>,
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
    pub text: Atom,
}

#[derive(Debug, Clone, Copy)]
pub struct TemplateSpan<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx self::Expr<'cx>,
    pub text: Atom,
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

/// ```txt
/// a[b]
/// ```
#[derive(Debug, Clone, Copy)]
pub struct EleAccessExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
    pub arg: &'cx Expr<'cx>,
}

/// ```txt
/// a.b
/// ```
#[derive(Debug, Clone, Copy)]
pub struct PropAccessExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
    pub question_dot: Option<Span>,
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

#[derive(Debug, Clone, Copy, PartialEq)]
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

pub fn has_rest_param(params: ParamsDecl) -> bool {
    params.last().is_some_and(|param| param.is_rest())
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
    Mod,
    PipePipe,
    Less,
    LessEq,
    Shl,
    Great,
    GreatEq,
    Shr,
    UShr,
    BitOr,
    BitAnd,
    BitXor,
    LogicalAnd,
    EqEq,
    EqEqEq,
    NEq,
    NEqEq,
    Instanceof,
    In,
    Satisfies,
    Exp,
    Comma,
}

impl BinOpKind {
    pub fn as_str(self) -> &'static str {
        use BinOpKind::*;
        match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Mod => "%",
            BitOr => "|",
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
            BitXor => "^",
            Comma => ",",
            Exp => "**",
        }
    }

    fn is_logical_op(self) -> bool {
        // TODO:  AmpersandAmpersand
        matches!(self, Self::PipePipe)
    }

    pub fn is_logical_or_coalescing_op(self) -> bool {
        // TODO: QuestionQuestion
        self.is_logical_op()
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
pub type BigIntLit = Lit<(bool, Atom)>;
pub type BoolLit = Lit<bool>;
pub type NullLit = Lit<()>;
pub type StringLit = Lit<Atom>;
pub type RegExpLit = Lit<Atom>;
pub type NoSubstitutionTemplateLit = Lit<Atom>;

#[derive(Debug, Clone, Copy)]
pub struct VarDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Binding<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
    pub init: Option<&'cx Expr<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Ident {
    pub id: NodeID,
    pub span: Span,
    pub name: Atom,
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
    pub expr: &'cx Expr<'cx>,
    pub ty_args: Option<&'cx self::Tys<'cx>>,
    pub args: Exprs<'cx>,
}

/// ```txt
/// let a = <string>'hello';
///         ^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, Clone, Copy)]
pub struct TyAssertion<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx self::Ty<'cx>,
    pub expr: &'cx Expr<'cx>,
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct RegularExpressionFlags: u8 {
        /// `d`
        const HAS_INDICES   = 1 << 0;
        /// `g`
        const GLOBAL        = 1 << 1;
        /// `i`
        const IGNORE_CASE   = 1 << 2;
        /// `m`
        const MULTILINE     = 1 << 3;
        /// `s`
        const DOT_ALL       = 1 << 4;
        /// `u`
        const UNICODE       = 1 << 5;
        /// `v`
        const UNICODE_SETS  = 1 << 6;
        /// `y`
        const STICKY        = 1 << 7;
        const ANY_UNICODE_MODE = Self::UNICODE.bits() | Self::UNICODE_SETS.bits();
        const MODIFIERS = Self::IGNORE_CASE.bits() | Self::MULTILINE.bits() | Self::DOT_ALL.bits();
    }
}

/// ```txt
/// tagExpr`something`
/// ```
#[derive(Debug, Clone, Copy)]
pub struct TaggedTemplateExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tag: &'cx Expr<'cx>,
    pub ty_args: Option<&'cx self::Tys<'cx>>,
    pub tpl: &'cx Expr<'cx>,
}
