use rts::parser::{Parser, ParserState};

fn main() {
    let ast_arena = bumpalo::Bump::new();
    let mut p = Parser::new(&ast_arena);
    let mut s = ParserState::new(&mut p, "1");
    let root = s.parse();
    dbg!(root);
    dbg!(p.parent_map);
}

#[test]
fn parse_0() {
    let ast_arena = bumpalo::Bump::new();
    let mut p = Parser::new(&ast_arena);
    let mut s = ParserState::new(&mut p, "1 + false");
    let program = s.parse();
    // insta::assert_debug_snapshot!((program, &p.parent_map));
    assert!(p.parent_map.parent(program.id).is_none());
    assert_eq!(p.parent_map.parent(program.stmts[0].id), Some(program.id));
    assert_eq!(program.id.as_u32(), 0);
}

#[test]
fn parse_1() {
    fn should_parse_success(input: &str) {
        let ast_arena = bumpalo::Bump::new();
        let mut p = Parser::new(&ast_arena);
        let mut s = ParserState::new(&mut p, input);
        let program = s.parse();
        assert!(p.parent_map.parent(program.id).is_none());
        assert_eq!(program.id.as_u32(), 0);
    }

    should_parse_success("1");
    should_parse_success("1234");
    should_parse_success("false");
}

#[test]
fn check_0() {
    fn should_check_success(input: &str) {
        let ast_arena = bumpalo::Bump::new();
        let mut p = Parser::new(&ast_arena);
        let mut s = ParserState::new(&mut p, input);
        let root = s.parse();
        let ty_arena = bumpalo::Bump::new();
        let mut c = rts::check::TyChecker::new(&ty_arena, &p.atoms);
        c.check_program(root);
        assert!(!c.tys.is_empty());
    }
    should_check_success("1");
}
