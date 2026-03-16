enum Operator {
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Eq,
}
struct Comparator {
    operator: Operator,
    operand: Version,
}
struct Version {
    major: u64,
    minor: u64,
    patch: u64,
    prerelease: Option<Vec<String>>,
    build: Option<Vec<String>>,
}

fn parse_range(text: &str) {
    let alternatives: Vec<Vec<Comparator>> = vec![vec![]];
    let text = text.trim();
    for range in text.split("||") {}
}
