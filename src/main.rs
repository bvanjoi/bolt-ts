use rts::parser::{TsParser, TsParserState};

fn main() {
    let ast_arena = bumpalo::Bump::new();
    let mut p = TsParser::new(&ast_arena);
    let mut s = TsParserState::new(&mut p, "1");
    s.parse();
    drop(s);
}

#[test]
fn parse() {
    let ast_arena = bumpalo::Bump::new();
    let mut p = TsParser::new(&ast_arena);
    {
        let mut s = TsParserState::new(&mut p, "1");
        let p = s.parse();
        assert_eq!(p.id.as_u32(), 0);
    }

    {
        let mut s = TsParserState::new(&mut p, "1234");
        let p = s.parse();
        assert_eq!(p.id.as_u32(), 4);
    }
}
