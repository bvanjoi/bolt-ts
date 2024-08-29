use rts::parser::TsParser;

fn main() {
    let mut p = TsParser::new();
    let node_map = p.parse("abc"); 
    let p = node_map.root();
    dbg!(p);
}
