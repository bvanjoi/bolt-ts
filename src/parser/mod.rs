mod ast;
mod keyword;
mod node;
mod node_flags;
mod scan;
mod token;

use ast::Program;
use keyword::{IDENTIFIER, KEYWORDS};
use node::{Node, NodeID};
use rustc_hash::FxHashMap;
use token::{BinPrec, Token, TokenKind};
use xxhash_rust::const_xxh3::xxh3_64;

use crate::span::Span;

pub struct NodeMap<'cx>(FxHashMap<NodeID, Node<'cx>>);

impl<'cx> NodeMap<'cx> {
    pub fn get(&self, id: NodeID) -> Node<'cx> {
        self.0.get(&id).copied().unwrap()
    }

    pub fn insert(&mut self, id: NodeID, node: Node<'cx>) {
        let prev = self.0.insert(id, node);
        assert!(prev.is_none())
    }
}

#[derive(Debug)]
pub struct ParentMap(FxHashMap<NodeID, NodeID>);

impl ParentMap {
    pub fn parent(&self, id: NodeID) -> Option<NodeID> {
        self.0.get(&id).copied()
    }

    pub fn insert(&mut self, id: NodeID, parent: NodeID) {
        let prev = self.0.insert(id, parent);
        assert!(prev.is_none())
    }
}

struct AtomMap(FxHashMap<AtomId, String>);

impl AtomMap {
    fn insert(&mut self, atom: AtomId, value: String) {
        let prev = self.0.insert(atom, value);
        assert!(prev.is_none());
    }

    fn get(&self, atom: AtomId) -> Option<&String> {
        self.0.get(&atom)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct AtomId(u64);

impl AtomId {
    pub const fn from_str(s: &str) -> Self {
        Self::from_bytes(s.as_bytes())
    }

    pub const fn from_bytes(bytes: &[u8]) -> Self {
        Self(xxh3_64(bytes))
    }
}

pub struct Parser<'cx> {
    arena: &'cx bumpalo::Bump,
    pub node_map: NodeMap<'cx>,
    pub parent_map: ParentMap,
    next_node_id: NodeID,
    atoms: AtomMap,
}

impl<'cx> Parser<'cx> {
    pub fn new(ast_arena: &'cx bumpalo::Bump) -> Self {
        let mut atoms = AtomMap(FxHashMap::default());
        for (atom, id) in KEYWORDS {
            atoms.insert(*id, atom.to_string());
        }
        for (atom, id) in IDENTIFIER {
            atoms.insert(*id, atom.to_string())
        }
        Self {
            node_map: NodeMap(FxHashMap::default()),
            parent_map: ParentMap(FxHashMap::default()),
            next_node_id: NodeID::root(),
            atoms,
            arena: ast_arena,
        }
    }

    fn next_node_id(&mut self) -> NodeID {
        let old = self.next_node_id;
        self.next_node_id = self.next_node_id.next();
        old
    }
}

pub struct ParserState<'cx, 'p> {
    p: &'p mut Parser<'cx>,
    input: &'p [u8],
    token: Token,
    token_number_value: Option<f64>,
    pos: usize,
    parent: NodeID,
}

impl<'cx, 'a, 'p> ParserState<'cx, 'p> {
    pub fn new(p: &'p mut Parser<'cx>, input: &'p str) -> Self {
        let token = Token::new(TokenKind::EOF, Span::new(u32::MAX, u32::MAX));
        let input = input.as_bytes();
        Self {
            input,
            token,
            token_number_value: None,
            pos: 0,
            p,
            parent: NodeID::root(),
        }
    }

    fn alloc<T>(&self, t: T) -> &'cx T {
        self.p.arena.alloc(t)
    }

    #[inline]
    fn end(&self) -> usize {
        self.input.len()
    }

    fn parse_stmt(&mut self) -> &'cx ast::Stmt<'cx> {
        let id = self.p.next_node_id();
        let stmt = self.with_parent(id, |this| {
            let kind = match this.token.kind {
                _ => ast::StmtKind::ExprStmt(this.parse_expr_or_labeled_stmt()),
            };
            let stmt = this.alloc(ast::Stmt { id, kind });
            stmt
        });
        self.insert_map(id, Node::Stmt(stmt));
        stmt
    }

    fn parse_expr_or_labeled_stmt(&mut self) -> &'cx ast::Expr<'cx> {
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> &'cx ast::Expr<'cx> {
        self.parse_assignment_expr()
    }

    fn parse_assignment_expr(&mut self) -> &'cx ast::Expr<'cx> {
        self.parse_binary_expr(BinPrec::Lowest)
    }

    fn parse_binary_expr(&mut self, prec: BinPrec) -> &'cx ast::Expr<'cx> {
        let start = self.token.start() as usize;
        let left = self.parse_unary_expr();
        self.parse_binary_expr_rest(prec, left, start)
    }

    fn parse_binary_expr_rest(
        &mut self,
        prec: BinPrec,
        left: &'cx ast::Expr<'_>,
        start: usize,
    ) -> &'cx ast::Expr<'cx> {
        let mut left = left;
        loop {
            let next_prec = self.token.kind.prec();
            if !(next_prec > prec) {
                break left;
            }
            let op = self.token.kind.into_binop();
            let expr_id = self.p.next_node_id();
            let kind = self.with_parent(expr_id, |this| {
                let bin_expr_id = this.p.next_node_id();
                this.next_token();
                let right = this.with_parent(bin_expr_id, |this| this.parse_binary_expr(next_prec));
                let bin_expr = this.alloc(ast::BinExpr {
                    id: bin_expr_id,
                    left,
                    op,
                    right,
                    span: Span::from((start, this.pos)),
                });
                this.insert_map(bin_expr_id, Node::BinExpr(bin_expr));
                ast::ExprKind::BinOp(bin_expr)
            });
            left = self.alloc(ast::Expr {
                id: expr_id,
                kind,
            });
            self.insert_map(expr_id, Node::Expr(left));
        }
    }

    fn parse_unary_expr(&mut self) -> &'cx ast::Expr<'cx> {
        if self.is_update_expr() {
            let start = self.token.start();
            let expr = self.parse_update_expr();
            return expr;
        }
        todo!()
    }

    fn parse_update_expr(&mut self) -> &'cx ast::Expr<'cx> {
        self.parse_left_hand_side_expr()
    }

    fn parse_left_hand_side_expr(&mut self) -> &'cx ast::Expr<'cx> {
        let start = self.token.start();
        self.parse_member_expr()
    }

    fn parse_member_expr(&mut self) -> &'cx ast::Expr<'cx> {
        let start = self.token.start();
        // TODO: member expr rest
        self.parse_primary_expr()
    }

    fn parse_primary_expr(&mut self) -> &'cx ast::Expr<'cx> {
        match self.token.kind {
            TokenKind::Number | TokenKind::False => self.parse_literal(),
            _ => todo!(),
        }
    }

    fn parse_literal(&mut self) -> &'cx ast::Expr<'cx> {
        let id = self.p.next_node_id();
        let expr = self.with_parent(id, |this| {
            let kind = match this.token.kind {
                TokenKind::Number => {
                    let num = this.token_number_value.unwrap();
                    let lit = this.create_lit(num, this.token.span);
                    this.insert_map(lit.id, Node::NumLit(lit));
                    ast::ExprKind::NumLit(lit)
                }
                TokenKind::False => {
                    let lit = this.create_lit(false, this.token.span);
                    this.insert_map(lit.id, Node::BoolLit(lit));
                    ast::ExprKind::BoolLit(lit)
                }
                _ => unreachable!(),
            };
            let expr = this.alloc(ast::Expr { id, kind });
            expr
        });
        self.insert_map(id, Node::Expr(expr));
        expr
    }

    fn is_update_expr(&self) -> bool {
        true
    }

    fn create_lit<T>(&mut self, val: T, span: Span) -> &'cx ast::Lit<T> {
        let id = self.p.next_node_id();
        let lit = self.alloc(ast::Lit { id, val, span });
        self.next_token();
        lit
    }

    #[inline]
    fn with_parent<T, F: FnOnce(&mut Self) -> T>(&mut self, parent: NodeID, f: F) -> T {
        let old = self.parent;
        self.parent = parent;
        let ret = f(self);
        self.parent = old;
        ret
    }

    fn insert_map(&mut self, id: NodeID, node: Node<'cx>) {
        self.p.node_map.insert(id, node);
        self.p.parent_map.insert(id, self.parent);
    }

    pub fn parse(&mut self) -> &'cx Program<'cx> {
        let id = self.p.next_node_id();
        self.with_parent(id, |this| {
            this.next_token();
            let stmts = this.p.arena.alloc(Vec::with_capacity(32));
            while !matches!(this.token.kind, TokenKind::EOF) {
                stmts.push(this.parse_stmt());
            }
            let program = this.alloc(Program { id, stmts });
            this.p.node_map.insert(id, Node::Program(program));
            program
        })
    }
}
