use bolt_ts_span::Span;

use crate::ast;
use crate::keyword;

use super::token::TokenKind;
use super::ParserState;
use super::{list_ctx, PResult};

#[derive(Copy, Clone)]
pub(super) struct ImportOrExportSpecs;
impl list_ctx::ListContext for ImportOrExportSpecs {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        use TokenKind::*;
        if s.token.kind == From
            && s.lookahead(|this| {
                this.next_token();
                matches!(this.token.kind, String)
            })
        {
            false
        } else if s.token.kind == String {
            true
        } else {
            s.token.kind.is_ident_or_keyword()
        }
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        use TokenKind::*;
        matches!(s.token.kind, RBrace)
    }
}

#[derive(Copy, Clone)]
pub(super) struct ParseNamedImports;
#[derive(Copy, Clone)]
pub(super) struct ParseNamedExports;

pub(super) trait ParseNamedImportsExports<'cx, 'p>: Copy {
    type Spec;
    fn parse_name(
        &self,
        state: &mut ParserState<'cx, 'p>,
        check_ident_is_keyword: &mut bool,
        check_ident_start: &mut u32,
        check_ident_end: &mut u32,
    ) -> &'cx ast::ModuleExportName<'cx>;
    fn finish_spec(
        &self,
        state: &mut ParserState<'cx, 'p>,
        id: ast::NodeID,
        span: Span,
        prop_name: Option<&'cx ast::ModuleExportName<'cx>>,
        name: &'cx ast::ModuleExportName<'cx>,
    ) -> Self::Spec;
}

impl<'cx, 'p> ParseNamedImportsExports<'cx, 'p> for ParseNamedImports {
    type Spec = &'cx ast::ImportSpec<'cx>;
    fn parse_name(
        &self,
        state: &mut ParserState<'cx, 'p>,
        check_ident_is_keyword: &mut bool,
        check_ident_start: &mut u32,
        check_ident_end: &mut u32,
    ) -> &'cx ast::ModuleExportName<'cx> {
        let ident = state.parse_name_with_kw_check(
            check_ident_is_keyword,
            check_ident_start,
            check_ident_end,
        );
        state.alloc(ast::ModuleExportName {
            kind: ast::ModuleExportNameKind::Ident(ident),
        })
    }
    fn finish_spec(
        &self,
        state: &mut ParserState<'cx, 'p>,
        id: ast::NodeID,
        span: Span,
        prop_name: Option<&'cx ast::ModuleExportName<'cx>>,
        name: &'cx ast::ModuleExportName<'cx>,
    ) -> Self::Spec {
        let kind = if let Some(prop_name) = prop_name {
            let ast::ModuleExportNameKind::Ident(ident) = prop_name.kind else {
                unreachable!()
            };
            let spec = state.alloc(ast::ImportNamedSpec {
                id,
                span,
                prop_name,
                name: ident,
            });
            state.insert_map(id, ast::Node::ImportNamedSpec(spec));
            ast::ImportSpecKind::Named(spec)
        } else {
            let ast::ModuleExportNameKind::Ident(ident) = name.kind else {
                unreachable!()
            };
            let spec = state.alloc(ast::ShorthandSpec {
                id,
                span,
                name: ident,
            });
            state.insert_map(id, ast::Node::ShorthandSpec(spec));
            ast::ImportSpecKind::ShortHand(spec)
        };
        state.alloc(ast::ImportSpec { kind })
    }
}
impl<'cx, 'p> ParseNamedImportsExports<'cx, 'p> for ParseNamedExports {
    type Spec = &'cx ast::ImportSpec<'cx>;
    fn parse_name(
        &self,
        state: &mut ParserState<'cx, 'p>,
        check_ident_is_keyword: &mut bool,
        check_ident_start: &mut u32,
        check_ident_end: &mut u32,
    ) -> &'cx ast::ModuleExportName<'cx> {
        state.parse_module_export_name(|this| {
            this.parse_name_with_kw_check(
                check_ident_is_keyword,
                check_ident_start,
                check_ident_end,
            )
        })
    }
    fn finish_spec(
        &self,
        state: &mut ParserState<'cx, 'p>,
        id: ast::NodeID,
        span: Span,
        prop_name: Option<&'cx ast::ModuleExportName<'cx>>,
        name: &'cx ast::ModuleExportName<'cx>,
    ) -> Self::Spec {
        todo!()
    }
}

impl<'cx, 'p> ParserState<'cx, 'p> {
    fn parse_module_export_name(
        &mut self,
        parse_name: impl FnOnce(&mut Self) -> &'cx ast::Ident,
    ) -> &'cx ast::ModuleExportName<'cx> {
        let kind = if self.token.kind == TokenKind::String {
            ast::ModuleExportNameKind::StringLit(self.parse_string_lit())
        } else {
            ast::ModuleExportNameKind::Ident(parse_name(self))
        };
        self.alloc(ast::ModuleExportName { kind })
    }

    fn parse_name_with_kw_check(
        &mut self,
        check_ident_is_keyword: &mut bool,
        check_ident_start: &mut u32,
        check_ident_end: &mut u32,
    ) -> &'cx ast::Ident {
        *check_ident_is_keyword = self.token.kind.is_keyword() && !self.token.kind.is_ident();
        *check_ident_start = self.token.start();
        *check_ident_end = self.token.end();
        let is_ident = self.token.kind.is_ident();
        self.create_ident(is_ident, None)
    }

    fn parse_import_or_export_spec<Spec>(
        &mut self,
        kind: impl ParseNamedImportsExports<'cx, 'p, Spec = Spec>,
    ) -> PResult<Spec> {
        let id = self.next_node_id();
        let start = self.token.start();
        let mut check_ident_is_keyword =
            self.token.kind.is_keyword() && !self.token.kind.is_ident();
        let mut check_ident_start = self.token.start();
        let mut check_ident_end = self.token.end();
        let mut is_type_only = false;
        let mut can_parse_as_keyword: bool = true;
        let mut prop_name = None;
        let mut name = self.parse_module_export_name(|this| this.create_ident(true, None));

        if let ast::ModuleExportNameKind::Ident(ident) = name.kind {
            if ident.name == keyword::KW_TYPE {
                if self.token.kind == TokenKind::As {
                    // { type as ... }
                    let first_as = self.create_ident(true, None);
                    if self.token.kind == TokenKind::As {
                        // { type as as ... }
                        let second_as = self.create_ident(true, None);
                        can_parse_as_keyword = false;
                        if self.token.kind.can_parse_module_export_name() {
                            // `{type as as ident}` or `{type as as "stringLit"}`
                            is_type_only = true;
                            prop_name = Some(self.alloc(ast::ModuleExportName {
                                kind: ast::ModuleExportNameKind::Ident(first_as),
                            }));
                            name = kind.parse_name(
                                self,
                                &mut check_ident_is_keyword,
                                &mut check_ident_start,
                                &mut check_ident_end,
                            );
                        } else {
                            // `{type as as}`
                            prop_name = Some(name);
                            name = self.alloc(ast::ModuleExportName {
                                kind: ast::ModuleExportNameKind::Ident(second_as),
                            });
                        }
                    } else if self.token.kind.can_parse_module_export_name() {
                        // `{type as ident}` or `{type as "stringLit"}`
                        prop_name = Some(name);
                        can_parse_as_keyword = false;
                        name = kind.parse_name(
                            self,
                            &mut check_ident_is_keyword,
                            &mut check_ident_start,
                            &mut check_ident_end,
                        );
                    } else {
                        // `{type as }`
                        is_type_only = true;
                        name = self.alloc(ast::ModuleExportName {
                            kind: ast::ModuleExportNameKind::Ident(first_as),
                        });
                    }
                } else if self.token.kind.can_parse_module_export_name() {
                    // `{type ident ... }` or `{type "stringLit" ...}`
                    is_type_only = true;
                    name = kind.parse_name(
                        self,
                        &mut check_ident_is_keyword,
                        &mut check_ident_start,
                        &mut check_ident_end,
                    );
                }
            }
        }

        if can_parse_as_keyword && self.token.kind == TokenKind::As {
            prop_name = Some(name);
            self.expect(TokenKind::As);
            name = kind.parse_name(
                self,
                &mut check_ident_is_keyword,
                &mut check_ident_start,
                &mut check_ident_end,
            );
        }
        let span = self.new_span(start);
        Ok(kind.finish_spec(self, id, span, prop_name, &name))
    }

    pub(super) fn parse_named_imports_or_exports<Spec>(
        &mut self,
        kind: impl ParseNamedImportsExports<'cx, 'p, Spec = Spec>,
    ) -> PResult<&'cx [Spec]> {
        // `{ ... }`
        self.parse_bracketed_list(
            ImportOrExportSpecs,
            TokenKind::LBrace,
            |this| this.parse_import_or_export_spec(kind),
            TokenKind::RBrace,
        )
    }
}
