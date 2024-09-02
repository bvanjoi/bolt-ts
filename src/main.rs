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
fn parse() {
    let ast_arena = bumpalo::Bump::new();
    let mut p = Parser::new(&ast_arena);
    {
        let mut s = ParserState::new(&mut p, "1");
        let program = s.parse();
        assert!(p.parent_map.parent(program.id).is_none());
        assert_eq!(p.parent_map.parent(program.stmts[0].id), Some(program.id));

        assert_eq!(program.id.as_u32(), 0);
    }

    {
        let mut s = ParserState::new(&mut p, "1234");
        let program = s.parse();
        assert_eq!(p.parent_map.parent(program.stmts[0].id), Some(program.id));
        assert_eq!(program.id.as_u32(), 4);
    }

    {
        let mut s = ParserState::new(&mut p, "false");
        let program = s.parse();
        assert_eq!(program.id.as_u32(), 8);
    }
}
