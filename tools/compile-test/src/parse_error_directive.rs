struct ErrorDirectiveParser<'a> {
    index: usize,
    bytes: &'a [u8],
    token: Option<Token>,

    revisions: Option<Vec<String>>,
    follow: Option<bool>,
    adjusts: Option<u8>,
}

#[derive(Debug)]
pub(super) struct ErrorDirective {
    revisions: Option<Vec<String>>,
    follow: bool,
    adjusts: u8,
    end: usize,
}

impl ErrorDirective {
    pub fn revisions(&self) -> Option<&[String]> {
        self.revisions.as_deref()
    }

    pub fn follow(&self) -> bool {
        self.follow
    }

    pub fn adjusts(&self) -> u8 {
        self.adjusts
    }

    pub fn end(&self) -> usize {
        self.end
    }
}

#[derive(PartialEq, Debug)]
enum Token {
    /// `//~`
    Start,
    /// `[`
    LeftBracket,
    /// `]`
    RightBracket,
    /// `|`
    Pipe,
    /// `^`
    Circumflexes(u8),
    /// `,`
    Comma,
}

impl ErrorDirectiveParser<'_> {
    fn scan(&mut self) -> Option<()> {
        if self.index >= self.bytes.len() {
            return None;
        }
        if self.bytes[self.index] == b' ' {
            self.index += 1;
            return self.scan();
        }
        if self.bytes[self.index..].starts_with(b"//~") {
            self.index += 3;
            self.token = Some(Token::Start);
            Some(())
        } else if self.bytes[self.index] == b'[' {
            self.index += 1;
            self.token = Some(Token::LeftBracket);
            Some(())
        } else if self.bytes[self.index] == b']' {
            self.index += 1;
            self.token = Some(Token::RightBracket);
            Some(())
        } else if self.bytes[self.index] == b'|' {
            self.index += 1;
            self.token = Some(Token::Pipe);
            Some(())
        } else if self.bytes[self.index] == b'^' {
            let mut count = 0;
            while self.index < self.bytes.len() && self.bytes[self.index] == b'^' {
                count += 1;
                self.index += 1;
            }
            self.token = Some(Token::Circumflexes(count));
            Some(())
        } else if self.bytes[self.index] == b',' {
            self.index += 1;
            self.token = Some(Token::Comma);
            Some(())
        } else {
            None
        }
    }

    fn parse_revisions(&mut self) {
        let mut revision = String::new();
        while let Some(ch) = self.bytes.get(self.index).copied() {
            if ch == b']' {
                self.revisions
                    .get_or_insert_with(Vec::new)
                    .push(revision.clone());
                return;
            } else if ch == b',' {
                self.index += 1;
                self.revisions
                    .get_or_insert_with(Vec::new)
                    .push(revision.clone());
                revision.clear();
            } else {
                revision.push(ch as char);
                self.index += 1;
            }
        }

        panic!("Expected closing bracket for revisions")
    }

    fn parse_revisions_or_adjust(&mut self) {
        if self.scan().is_none() {
            return;
        }

        if let Some(Token::LeftBracket) = self.token {
            self.parse_revisions();
            self.scan();
            assert!(self.token == Some(Token::RightBracket));
            self.scan();
        }

        match self.token {
            Some(Token::Pipe) => {
                self.follow = Some(true);
                self.adjusts = Some(0);
            }
            Some(Token::Circumflexes(count)) => {
                self.follow = Some(false);
                self.adjusts = Some(count as u8);
            }
            None => return,
            _ => unreachable!(),
        }
    }

    fn parse(&mut self) -> Option<ErrorDirective> {
        self.scan()?;
        assert_eq!(self.token, Some(Token::Start));
        self.parse_revisions_or_adjust();
        Some(ErrorDirective {
            revisions: self.revisions.take(),
            follow: self.follow.take().unwrap_or(false),
            adjusts: self.adjusts.take().unwrap_or(0),
            end: self.index,
        })
    }
}

/// Matches comments like:
///     `//~`
///     `//~|`
///     `//~^`
///     `//~^^^^^`
///     `//~[rev1]`
///     `//~[rev1,rev2]^^`
pub(super) fn parse_error_directive(line: &str) -> Option<ErrorDirective> {
    let index = line.find("//~")?;
    let mut parser = ErrorDirectiveParser {
        index,
        bytes: line.as_bytes(),
        revisions: None,
        follow: None,
        adjusts: None,
        token: None,
    };
    parser.parse()
}

#[test]
fn test_parse_error_directive_0() {
    let line = "//~ ERROR: abc";
    let directive = parse_error_directive(line).unwrap();
    assert_eq!(directive.revisions, None);
    assert_eq!(directive.follow, false);
    assert_eq!(directive.adjusts, 0);
    assert_eq!(directive.end, 4);
}

#[test]
fn test_parse_error_directive_1() {
    let line = "//~| ERROR: abc";
    let directive = parse_error_directive(line).unwrap();
    assert_eq!(directive.revisions, None);
    assert_eq!(directive.follow, true);
    assert_eq!(directive.adjusts, 0);
    assert_eq!(directive.end, 4);
}

#[test]
fn test_parse_error_directive_2() {
    let line = "//~^ ERROR: abc";
    let directive = parse_error_directive(line).unwrap();
    assert_eq!(directive.revisions, None);
    assert_eq!(directive.follow, false);
    assert_eq!(directive.adjusts, 1);
    assert_eq!(directive.end, 4);
}

#[test]
fn test_parse_error_directive_3() {
    let line = "//~^^ ERROR: abc";
    let directive = parse_error_directive(line).unwrap();
    assert_eq!(directive.revisions, None);
    assert_eq!(directive.follow, false);
    assert_eq!(directive.adjusts, 2);
    assert_eq!(directive.end, 5);
}

#[test]
fn test_parse_error_directive_4() {
    let line = "abcdefg //~ ERROR: abc";
    let directive = parse_error_directive(line).unwrap();
    assert_eq!(directive.revisions, None);
    assert_eq!(directive.follow, false);
    assert_eq!(directive.adjusts, 0);
    assert_eq!(directive.end, 12);
}

#[test]
fn test_parse_error_directive_with_revision_0() {
    let line = "//~[a,b,c]^^ ERROR: abc";
    let directive = parse_error_directive(line).unwrap();
    assert_eq!(
        directive.revisions,
        Some(vec!["a".to_string(), "b".to_string(), "c".to_string()])
    );
    assert_eq!(directive.follow, false);
    assert_eq!(directive.adjusts, 2);
    assert_eq!(directive.end, 12);
}
