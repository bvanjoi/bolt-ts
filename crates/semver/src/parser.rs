use super::RangeSet;
use super::{Build, BuildId, Comparator, ComparatorKind};
use super::{HyphenRange, PreRelease, PreReleasePart, Range, Version, VersionPart};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedEof,
    UnexpectedChar(char),
    InvalidNumber(String),
    EmptyVersion,
    InvalidPreRelease,
    InvalidBuild,
    InvalidOperator,
    InvalidRangeSyntax,
    InvalidUtf8,
}
pub(super) struct Parser<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        Self {
            bytes: input,
            pos: 0,
        }
    }

    #[inline]
    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    #[inline]
    fn next(&mut self) -> Option<u8> {
        let b = self.bytes.get(self.pos).copied();
        if b.is_some() {
            self.pos += 1;
        }
        b
    }

    #[inline]
    fn consume_whitespace(&mut self) {
        while let Some(b) = self.peek() {
            if b.is_ascii_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.pos >= self.bytes.len()
    }

    fn parse_xr(&mut self) -> Result<VersionPart, ParseError> {
        self.consume_whitespace();

        match self.peek() {
            Some(b'x') | Some(b'X') | Some(b'*') => {
                self.next();
                Ok(VersionPart::Wildcard)
            }
            Some(c) if c.is_ascii_digit() => {
                let start = self.pos;
                while let Some(c) = self.peek() {
                    if c.is_ascii_digit() {
                        self.next();
                    } else {
                        break;
                    }
                }

                // SAFETY: We know it's ASCII digits
                let num_str =
                    unsafe { std::str::from_utf8_unchecked(&self.bytes[start..self.pos]) };

                if num_str == "0" || !num_str.starts_with('0') {
                    num_str
                        .parse::<u64>()
                        .map(VersionPart::Number)
                        .map_err(|_| ParseError::InvalidNumber(num_str.to_string()))
                } else {
                    Err(ParseError::InvalidNumber(num_str.to_string()))
                }
            }
            None => Err(ParseError::UnexpectedEof),
            Some(c) => Err(ParseError::UnexpectedChar(c as char)),
        }
    }



    fn parse_prepart(&mut self) -> Result<PreReleasePart, ParseError> {
        self.consume_whitespace();

        let start = self.pos;
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == b'-' {
                self.next();
            } else {
                break;
            }
        }

        if self.pos == start {
            return Err(ParseError::InvalidPreRelease);
        }

        // SAFETY: We know it's ASCII
        let s = unsafe { std::str::from_utf8_unchecked(&self.bytes[start..self.pos]) };

        // Check if it's numeric
        if s.chars().all(|c| c.is_ascii_digit()) {
            if s == "0" || !s.starts_with('0') {
                s.parse::<u64>()
                    .map(PreReleasePart::Numeric)
                    .map_err(|_| ParseError::InvalidNumber(s.to_string()))
            } else {
                Err(ParseError::InvalidNumber(s.to_string()))
            }
        } else {
            // Must contain at least one non-digit
            if s.chars().all(|c| c.is_ascii_digit()) {
                return Err(ParseError::InvalidPreRelease);
            }
            Ok(PreReleasePart::AlphaNumeric(s.to_string()))
        }
    }

    fn parse_pre(&mut self) -> Result<PreRelease, ParseError> {
        let mut parts = Vec::new();

        parts.push(self.parse_prepart()?);

        while let Some(b'.') = self.peek() {
            self.next(); // consume '.'
            parts.push(self.parse_prepart()?);
        }

        Ok(PreRelease(parts))
    }

    fn parse_build_id(&mut self) -> Result<BuildId, ParseError> {
        self.consume_whitespace();

        let start = self.pos;
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == b'-' {
                self.next();
            } else {
                break;
            }
        }

        if self.pos == start {
            return Err(ParseError::InvalidBuild);
        }

        // Convert to string, validating UTF-8
        let slice = &self.bytes[start..self.pos];
        let s = std::str::from_utf8(slice)
            .map(|s| s.to_string())
            .map_err(|_| ParseError::InvalidUtf8)?;

        Ok(BuildId(s))
    }

    fn parse_build(&mut self) -> Result<Build, ParseError> {
        let mut ids = Vec::new();

        ids.push(self.parse_build_id()?);

        while let Some(b'.') = self.peek() {
            self.next(); // consume '.'
            ids.push(self.parse_build_id()?);
        }

        Ok(Build(ids))
    }

    fn parse_partial(&mut self) -> Result<Version, ParseError> {
        self.consume_whitespace();

        let major = self.parse_xr()?;
        let mut minor = None;
        let mut patch = None;

        // Parse minor if present
        if let Some(b'.') = self.peek() {
            self.next(); // consume '.'
            minor = Some(self.parse_xr()?);

            // Parse patch if present
            if let Some(b'.') = self.peek() {
                self.next(); // consume '.'
                patch = Some(self.parse_xr()?);
            }
        }

        // Parse optional qualifier
        let mut pre = None;
        let mut build = None;

        if let Some(b'-') = self.peek() {
            self.next(); // consume '-'
            pre = Some(self.parse_pre()?);
        }

        if let Some(b'+') = self.peek() {
            self.next(); // consume '+'
            build = Some(self.parse_build()?);
        }

        Ok(Version {
            major,
            minor,
            patch,
            pre_release: pre,
            build,
        })
    }

    fn parse_primitive(&mut self) -> Result<Comparator, ParseError> {
        self.consume_whitespace();

        let kind = match self.next() {
            Some(b'<') => {
                if let Some(b'=') = self.peek() {
                    self.next();
                    ComparatorKind::LessEq
                } else {
                    ComparatorKind::Less
                }
            }
            Some(b'>') => {
                if let Some(b'=') = self.peek() {
                    self.next();
                    ComparatorKind::GreaterEq
                } else {
                    ComparatorKind::Greater
                }
            }
            Some(b'=') => ComparatorKind::Eq,
            _ => return Err(ParseError::InvalidOperator),
        };

        let version = self.parse_partial()?;
        Ok(Comparator { kind, version })
    }

    fn parse_tilde(&mut self) -> Result<Comparator, ParseError> {
        self.consume_whitespace();

        if self.next() != Some(b'~') {
            return Err(ParseError::InvalidOperator);
        }

        let version = self.parse_partial()?;
        Ok(Comparator {
            kind: ComparatorKind::Tilde,
            version,
        })
    }

    fn parse_caret(&mut self) -> Result<Comparator, ParseError> {
        self.consume_whitespace();

        if self.next() != Some(b'^') {
            return Err(ParseError::InvalidOperator);
        }

        let version = self.parse_partial()?;
        Ok(Comparator {
            kind: ComparatorKind::Caret,
            version,
        })
    }

    fn parse_comparator(&mut self) -> Result<Comparator, ParseError> {
        self.consume_whitespace();

        let saved_pos = self.pos;

        if let Ok(prim) = self.parse_primitive() {
            return Ok(prim);
        }

        self.pos = saved_pos;
        if let Some(b'~') = self.peek() {
            return self.parse_tilde();
        }

        self.pos = saved_pos;
        if let Some(b'^') = self.peek() {
            return self.parse_caret();
        }

        self.pos = saved_pos;
        let version = self.parse_partial()?;
        Ok(Comparator {
            kind: ComparatorKind::Exact,
            version,
        })
    }

    fn parse_hyphen(&mut self) -> Result<HyphenRange, ParseError> {
        self.consume_whitespace();

        let from = self.parse_partial()?;

        self.consume_whitespace();
        if self.next() != Some(b'-') {
            return Err(ParseError::InvalidRangeSyntax);
        }
        self.consume_whitespace();

        let to = self.parse_partial()?;

        Ok(HyphenRange { from, to })
    }

    fn parse_range(&mut self) -> Result<Range, ParseError> {
        self.consume_whitespace();

        if self.is_eof() {
            return Ok(Range::Any);
        }

        let saved_pos = self.pos;

        if let Ok(hyphen) = self.parse_hyphen() {
            return Ok(Range::Hyphen(hyphen));
        }

        self.pos = saved_pos;
        let mut simples = Vec::new();

        match self.parse_comparator() {
            Ok(simple) => simples.push(simple),
            Err(_) if simples.is_empty() => return Ok(Range::Any),
            Err(e) => return Err(e),
        }

        loop {
            let before_whitespace = self.pos;
            self.consume_whitespace();

            if self.is_eof() {
                break;
            }

            let _before_simple = self.pos;
            match self.parse_comparator() {
                Ok(simple) => simples.push(simple),
                Err(_) => {
                    // Not a simple, backtrack
                    self.pos = before_whitespace;
                    break;
                }
            }
        }

        Ok(Range::Comparators(simples))
    }

    fn parse_range_set(&mut self) -> Result<RangeSet, ParseError> {
        let mut ranges = Vec::new();

        loop {
            self.consume_whitespace();

            if self.is_eof() {
                break;
            }

            // Parse a range
            let range = self.parse_range()?;
            ranges.push(range);

            self.consume_whitespace();

            // Check for logical OR
            if let Some(b'|') = self.peek() {
                self.next(); // consume first '|'
                if self.next() != Some(b'|') {
                    return Err(ParseError::InvalidRangeSyntax);
                }
                // Continue to next range
            } else {
                break;
            }
        }

        // Check for trailing characters
        self.consume_whitespace();
        if !self.is_eof() {
            return Err(ParseError::InvalidRangeSyntax);
        }

        Ok(RangeSet { ranges })
    }
}

pub fn parse_range(input: &str) -> Result<RangeSet, ParseError> {
    let mut parser = Parser::new(input.as_bytes());
    parser.parse_range_set()
}

pub fn parse_version(input: &str) -> Result<Version, ParseError> {
    let mut parser = Parser::new(input.as_bytes());
    parser.parse_partial()
}
