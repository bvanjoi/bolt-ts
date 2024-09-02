mod ast;
mod keyword;
mod node;
mod node_flags;
mod scan;
mod token;

use ast::{NumLit, Program};
use keyword::{IDENTIFIER, KEYWORD};
use node::{Node, NodeID};
use rustc_hash::FxHashMap;
use token::{Token, TokenKind};

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

    fn get(&self, atom: AtomId) -> &str {
        self.0.get(&atom).unwrap()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct AtomId(u64);

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
        for (atom, id) in KEYWORD {
            atoms.insert(*id, atom.to_string())
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
        self.parse_binary_expr()
    }

    fn parse_binary_expr(&mut self) -> &'cx ast::Expr<'cx> {
        self.parse_unary_expr()
    }

    fn parse_unary_expr(&mut self) -> &'cx ast::Expr<'cx> {
        if self.is_update_expr() {
            let pos = self.token.start();
            let expr = self.parse_update_expr();
            return expr;
        }
        todo!()
    }

    fn parse_update_expr(&mut self) -> &'cx ast::Expr<'cx> {
        self.parse_left_hand_side_expr()
    }

    fn parse_left_hand_side_expr(&mut self) -> &'cx ast::Expr<'cx> {
        let pos = self.token.start();
        self.parse_member_expr()
    }

    fn parse_member_expr(&mut self) -> &'cx ast::Expr<'cx> {
        let pos = self.token.start();
        // TODO: member expr rest
        self.parse_primary_expr()
    }

    fn parse_primary_expr(&mut self) -> &'cx ast::Expr<'cx> {
        match self.token.kind {
            TokenKind::Number(_) => self.parse_literal(),
            _ => todo!(),
        }
    }

    fn parse_literal(&mut self) -> &'cx ast::Expr<'cx> {
        let id = self.p.next_node_id();
        let expr = self.with_parent(id, |this| {
            let kind = match this.token.kind {
                TokenKind::Number(num) => {
                    ast::ExprKind::NumLit(this.create_numeric_literal(num, this.token.span))
                }
                _ => unreachable!(),
            };
            let expr = this.alloc(ast::Expr { id, kind });
            this.next_token();
            expr
        });
        self.insert_map(id, Node::Expr(expr));
        expr
    }

    fn is_update_expr(&self) -> bool {
        true
    }

    fn create_numeric_literal(&mut self, num: f64, span: Span) -> &'cx NumLit {
        let id = self.p.next_node_id();
        let lit = self.alloc(ast::NumLit { id, num, span });
        self.insert_map(id, Node::NumLit(lit));
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
