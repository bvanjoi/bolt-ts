use super::Emit;
use bolt_ts_ast as ast;

pub(super) trait ElemLike<'cx> {
    fn emit_item(&self, emitter: &mut Emit<'cx>);
    fn emit_sep(&self, emitter: &mut Emit<'cx>);
}
pub(super) trait ElemsLike<'cx> {
    type Elem: ElemLike<'cx>;
    fn is_empty(&self) -> bool;
    fn elems(&self) -> &[&Self::Elem];
}

pub(super) trait BlockLike<'cx> {
    type Elems: ElemsLike<'cx>;
    fn elems(&self) -> &Self::Elems;
}

impl<'cx> ElemLike<'cx> for ast::ClassElem<'cx> {
    fn emit_item(&self, emitter: &mut Emit<'cx>) {
        emitter.emit_class_ele(self);
    }
    fn emit_sep(&self, emitter: &mut Emit<'cx>) {
        if !matches!(self.kind, ast::ClassEleKind::IndexSig(_)) {
            emitter.content.p_newline();
        }
    }
}
impl<'cx> ElemsLike<'cx> for &'cx [&'cx ast::ClassElem<'cx>] {
    type Elem = ast::ClassElem<'cx>;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    fn elems(&self) -> &[&Self::Elem] {
        self
    }
}

impl<'cx> BlockLike<'cx> for ast::ClassElems<'cx> {
    type Elems = &'cx [&'cx ast::ClassElem<'cx>];

    fn elems(&self) -> &Self::Elems {
        &self.elems
    }
}

impl<'cx> ElemLike<'cx> for ast::Stmt<'cx> {
    fn emit_item(&self, emitter: &mut Emit<'cx>) {
        emitter.emit_stmt(self);
    }
    fn emit_sep(&self, emitter: &mut Emit<'cx>) {
        emitter.content.p_newline();
    }
}
impl<'cx> ElemsLike<'cx> for ast::Stmts<'cx> {
    type Elem = ast::Stmt<'cx>;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    fn elems(&self) -> &[&Self::Elem] {
        self
    }
}

// block stmt
impl<'cx> BlockLike<'cx> for ast::BlockStmt<'cx> {
    type Elems = ast::Stmts<'cx>;
    fn elems(&self) -> &Self::Elems {
        &self.stmts
    }
}

impl<'cx> Emit<'cx> {
    pub(super) fn emit_block_like(&mut self, block: &impl BlockLike<'cx>) {
        self.content.p_l_brace();
        if !block.elems().is_empty() {
            self.content.indent += self.options.indent;
            self.content.p_newline();
        }
        self.emit_list(
            block.elems().elems(),
            |this, elem| elem.emit_item(this),
            |this, elem| elem.emit_sep(this),
        );
        if !block.elems().is_empty() {
            self.content.indent -= self.options.indent;
            self.content.p_newline();
        }
        self.content.p_r_brace();
    }
}
