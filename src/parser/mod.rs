mod ast;
mod node;
mod node_flags;
mod scan;
mod token;

use ast::{Expr, NumLit, Program};
use node::{Node, NodeID};
use rustc_hash::FxHashMap;
use token::{Token, TokenFlags, TokenKind};

use crate::span::Span;

pub struct NodeMap<'cx> {
    node_map: NodeID2Node<'cx>,
    parent_map: FxHashMap<NodeID, NodeID>,
}

impl<'cx> NodeMap<'cx> {
    pub fn get(&self, id: NodeID) -> &Node<'cx> {
        self.node_map.0.get(&id).unwrap()
    }

    pub fn parent(&self, id: NodeID) -> Option<NodeID> {
        self.parent_map.get(&id).copied()
    }
}

#[derive(Default, Debug)]
pub struct NodeID2Node<'cx>(FxHashMap<NodeID, Node<'cx>>);
impl<'cx> NodeID2Node<'cx> {
    pub fn insert(&mut self, id: NodeID, node: Node<'cx>) {
        let prev = self.0.insert(id, node);
        assert!(prev.is_none());
    }
}

#[derive(Debug)]
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

pub struct TsParser<'cx> {
    arena: &'cx bumpalo::Bump,
    pub node_map: NodeID2Node<'cx>,
    parent_map: FxHashMap<NodeID, NodeID>,
    next_node_id: NodeID,
    atoms: AtomMap,
}

fn alloc<T>(arena: &bumpalo::Bump, t: T) -> &T {
    arena.alloc(t)
}

impl<'cx> TsParser<'cx> {
    pub fn new(ast_arena: &'cx bumpalo::Bump) -> Self {
        Self {
            node_map: NodeID2Node(FxHashMap::default()),
            parent_map: FxHashMap::default(),
            next_node_id: NodeID::root(),
            atoms: AtomMap(FxHashMap::default()),
            arena: ast_arena,
        }
    }

    fn next_node_id(&mut self) -> NodeID {
        let old = self.next_node_id;
        self.next_node_id = self.next_node_id.next();
        old
    }
}

pub struct TsParserState<'cx, 'p> {
    p: &'p mut TsParser<'cx>,
    input: &'p [u8],
    token: Token,
    pos: usize,
}

impl<'cx, 'a, 'p> TsParserState<'cx, 'p> {
    pub fn new(p: &'p mut TsParser<'cx>, input: &'p str) -> Self {
        let token = Token::new(TokenKind::EOF, Span::new(u32::MAX, u32::MAX));
        let input = input.as_bytes();
        Self {
            input,
            token,
            pos: 0,
            p,
        }
    }

    fn parse_stmt(&mut self) -> &'cx ast::Stmt<'cx> {
        let id = self.p.next_node_id();
        let kind = match self.token.kind {
            _ => ast::StmtKind::ExprStmt(self.parse_expr_or_labeled_stmt()),
        };
        let stmt = ast::Stmt { id, kind };
        let stmt = alloc(self.p.arena, stmt);
        self.p.node_map.insert(id, Node::Stmt(stmt));
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
        let kind = match self.token.kind {
            TokenKind::Number(num) => {
                ast::ExprKind::NumLit(self.create_numeric_literal(num, self.token.span))
            }
            _ => unreachable!(),
        };
        let expr = ast::Expr { id, kind };
        let expr = alloc(self.p.arena, expr);
        self.p.node_map.insert(id, Node::Expr(expr));
        self.next_token();
        expr
    }

    fn is_update_expr(&self) -> bool {
        true
    }

    fn create_numeric_literal(&mut self, num: f64, span: Span) -> &'cx NumLit {
        let id = self.p.next_node_id();
        let lit = alloc(self.p.arena, ast::NumLit { num, span });
        self.p.node_map.insert(id, Node::NumLit(lit));
        lit
    }

    pub fn parse(&mut self) -> &'cx Program<'cx> {
        let id = self.p.next_node_id();
        self.next_token();

        let stmts = self.p.arena.alloc(Vec::with_capacity(32));
        while !matches!(self.token.kind, TokenKind::EOF) {
            stmts.push(self.parse_stmt());
        }

        let program = self.p.arena.alloc(Program { id, stmts });
        self.p.node_map.insert(id, Node::Program(program));
        program
    }
}
