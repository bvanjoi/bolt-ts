mod parser;
mod test;

use std::fmt;

pub use self::parser::ParseError;
pub use self::parser::parse_range;
pub use self::parser::parse_version;
use self::test::test_disjunction;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VersionPart {
    Wildcard, // x, X, *
    Number(u64),
}

impl VersionPart {
    fn compare_to(&self, other: &VersionPart) -> std::cmp::Ordering {
        match (self, other) {
            (VersionPart::Wildcard, VersionPart::Wildcard) => std::cmp::Ordering::Equal,
            (VersionPart::Wildcard, _) => std::cmp::Ordering::Greater,
            (_, VersionPart::Wildcard) => std::cmp::Ordering::Less,
            (VersionPart::Number(a), VersionPart::Number(b)) => a.cmp(b),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Version {
    pub major: VersionPart,
    pub minor: Option<VersionPart>,
    pub patch: Option<VersionPart>,
    pub pre_release: Option<PreRelease>,
    pub build: Option<Build>,
}

impl Version {
    fn compare_to(&self, other: &Version) -> std::cmp::Ordering {
        // self.major
        if self == other {
            return std::cmp::Ordering::Equal;
        }
        let major_cmp = self.major.compare_to(&other.major);
        if major_cmp != std::cmp::Ordering::Equal {
            return major_cmp;
        }
        let minor_cmp = match (&self.minor, &other.minor) {
            (None, None) => std::cmp::Ordering::Equal,
            (None, Some(other)) => {
                if VersionPart::Number(0).eq(other) {
                    std::cmp::Ordering::Equal
                } else {
                    std::cmp::Ordering::Greater
                }
            }
            (Some(this), None) => {
                if VersionPart::Number(0).eq(this) {
                    std::cmp::Ordering::Equal
                } else {
                    std::cmp::Ordering::Less
                }
            }
            (Some(a), Some(b)) => a.compare_to(b),
        };
        if minor_cmp != std::cmp::Ordering::Equal {
            return minor_cmp;
        }
        let patch_cmp = match (&self.patch, &other.patch) {
            (None, None) => std::cmp::Ordering::Equal,
            (None, Some(other)) => {
                if VersionPart::Number(0).eq(other) {
                    std::cmp::Ordering::Equal
                } else {
                    std::cmp::Ordering::Greater
                }
            }
            (Some(this), None) => {
                if VersionPart::Number(0).eq(this) {
                    std::cmp::Ordering::Equal
                } else {
                    std::cmp::Ordering::Less
                }
            }
            (Some(a), Some(b)) => a.compare_to(b),
        };
        if patch_cmp != std::cmp::Ordering::Equal {
            return patch_cmp;
        }
        match (&self.pre_release, &other.pre_release) {
            (None, None) => std::cmp::Ordering::Equal,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (Some(_), None) => std::cmp::Ordering::Less,
            (Some(a), Some(b)) => a.compare_to(b),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PreReleasePart {
    Numeric(u64),
    AlphaNumeric(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuildId(String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreRelease(pub Vec<PreReleasePart>);

impl PreRelease {
    fn compare_to(&self, other: &PreRelease) -> std::cmp::Ordering {
        if self.0.is_empty() {
            if other.0.is_empty() {
                return std::cmp::Ordering::Equal;
            } else {
                return std::cmp::Ordering::Greater;
            }
        } else if other.0.is_empty() {
            return std::cmp::Ordering::Less;
        }
        let len = std::cmp::min(self.0.len(), other.0.len());
        for i in 0..len {
            let a = &self.0[i];
            let b = &other.0[i];
            if a == b {
                continue;
            }
            match (a, b) {
                (PreReleasePart::Numeric(a), PreReleasePart::Numeric(b)) => {
                    let ordering = a.cmp(b);
                    if ordering != std::cmp::Ordering::Equal {
                        return ordering;
                    }
                }
                (PreReleasePart::Numeric(_), PreReleasePart::AlphaNumeric(_)) => {
                    return std::cmp::Ordering::Less;
                }
                (PreReleasePart::AlphaNumeric(_), PreReleasePart::Numeric(_)) => {
                    return std::cmp::Ordering::Greater;
                }
                (PreReleasePart::AlphaNumeric(a), PreReleasePart::AlphaNumeric(b)) => {
                    let ordering = a.cmp(b);
                    if ordering != std::cmp::Ordering::Equal {
                        return ordering;
                    }
                }
            }
        }
        self.0.len().cmp(&other.0.len())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Build(Vec<BuildId>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparatorKind {
    /// `<`
    Less,
    /// `>`
    Greater,
    /// `<=`
    LessEq,
    /// `>=`
    GreaterEq,
    /// `=`
    Eq,
    /// `~`    
    Tilde,
    /// `^`
    Caret,
    Exact,
}

impl ComparatorKind {
    fn ast_str(&self) -> &str {
        match self {
            ComparatorKind::Less => "<",
            ComparatorKind::Greater => ">",
            ComparatorKind::LessEq => "<=",
            ComparatorKind::GreaterEq => ">=",
            ComparatorKind::Eq => "=",
            ComparatorKind::Tilde => "~",
            ComparatorKind::Caret => "^",
            ComparatorKind::Exact => "",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Comparator {
    kind: ComparatorKind,
    version: Version,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HyphenRange {
    pub from: Version,
    pub to: Version,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Range {
    Hyphen(HyphenRange),
    Comparators(Vec<Comparator>),
    Any, // Empty string
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RangeSet {
    pub ranges: Vec<Range>, // OR of ranges
}

impl RangeSet {
    pub fn test(&self, version: &Version) -> bool {
        test_disjunction(version, self)
    }
}

// Helper implementations for display
impl fmt::Display for VersionPart {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VersionPart::Wildcard => write!(f, "*"),
            VersionPart::Number(n) => write!(f, "{}", n),
        }
    }
}

impl fmt::Display for PreReleasePart {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PreReleasePart::Numeric(n) => write!(f, "{}", n),
            PreReleasePart::AlphaNumeric(s) => write!(f, "{}", s),
        }
    }
}

impl fmt::Display for BuildId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for PreRelease {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, part) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ".")?;
            }
            write!(f, "{}", part)?;
        }
        Ok(())
    }
}

impl fmt::Display for Build {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, id) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ".")?;
            }
            write!(f, "{}", id)?;
        }
        Ok(())
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.major)?;

        if let Some(minor) = &self.minor {
            write!(f, ".{}", minor)?;
        }

        if let Some(patch) = &self.patch {
            write!(f, ".{}", patch)?;
        }

        if let Some(pre) = &self.pre_release {
            write!(f, "-{}", pre)?;
        }

        if let Some(build) = &self.build {
            write!(f, "+{}", build)?;
        }

        Ok(())
    }
}



impl fmt::Display for Comparator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.kind, self.version)
    }
}

impl fmt::Display for ComparatorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ast_str())
    }
}

impl fmt::Display for HyphenRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} - {}", self.from, self.to)
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Range::Hyphen(hyphen) => write!(f, "{}", hyphen),
            Range::Comparators(simples) => {
                for (i, simple) in simples.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", simple)?;
                }
                Ok(())
            }
            Range::Any => Ok(()), // Empty string
        }
    }
}

impl fmt::Display for RangeSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, range) in self.ranges.iter().enumerate() {
            if i > 0 {
                write!(f, " || ")?;
            }
            write!(f, "{}", range)?;
        }
        Ok(())
    }
}

#[test]
fn test_version() {
    let version = parse_version("1.2.3-pre.4+build.5").unwrap();
    assert!(version.to_string().eq("1.2.3-pre.4+build.5"));
    assert_eq!(version.major, VersionPart::Number(1));
    assert_eq!(version.minor, Some(VersionPart::Number(2)));
    assert_eq!(version.patch, Some(VersionPart::Number(3)));
    assert_eq!(
        version.pre_release.unwrap(),
        PreRelease(vec![
            PreReleasePart::AlphaNumeric("pre".to_string()),
            PreReleasePart::Numeric(4),
        ])
    );
    assert_eq!(
        version.build.unwrap(),
        Build(vec![BuildId("build".to_string()), BuildId("5".to_string()),])
    );
}

#[test]
fn test_compare_to() {
    let v1 = parse_version("1.0.0").unwrap();
    let v2 = parse_version("2.0.0").unwrap();
    let v3 = parse_version("1.1.0").unwrap();
    let v4 = parse_version("1.0.1").unwrap();
    let v5 = parse_version("1.0.0-pre").unwrap();
    let v6 = parse_version("1.0.1-pre").unwrap();
    let v7 = parse_version("1.0.0-0").unwrap();
    let v8 = parse_version("1.0.0-1").unwrap();
    let v9 = parse_version("1.0.0-2").unwrap();
    let v10 = parse_version("1.0.0-10").unwrap();
    let v11 = parse_version("1.0.0-a").unwrap();
    let v12 = parse_version("1.0.0-b").unwrap();
    let v13 = parse_version("1.0.0-a-2").unwrap();
    let v14 = parse_version("1.0.0-a-10").unwrap();
    let v15 = parse_version("1.0.0-A").unwrap();
    let v16 = parse_version("1.0.0-alpha").unwrap();
    let v17 = parse_version("1.0.0-alpha.0").unwrap();
    let v18 = parse_version("1.0.0-a.0.b.1").unwrap();
    let v19 = parse_version("1.0.0-a.0.b.2").unwrap();
    let v20 = parse_version("1.0.0-b.0.a.1").unwrap();
    let v21 = parse_version("1.0.0+build").unwrap();

    assert_eq!(v1.compare_to(&v2), std::cmp::Ordering::Less);
    assert_eq!(v2.compare_to(&v1), std::cmp::Ordering::Greater);
    assert_eq!(v1.compare_to(&v3), std::cmp::Ordering::Less);
    assert_eq!(v3.compare_to(&v1), std::cmp::Ordering::Greater);
    assert_eq!(v1.compare_to(&v4), std::cmp::Ordering::Less);
    assert_eq!(v4.compare_to(&v1), std::cmp::Ordering::Greater);
    assert_eq!(v1.compare_to(&v1), std::cmp::Ordering::Equal);
    assert_eq!(v1.compare_to(&v5), std::cmp::Ordering::Greater);
    assert_eq!(v5.compare_to(&v1), std::cmp::Ordering::Less);
    assert_eq!(v6.compare_to(&v1), std::cmp::Ordering::Greater);
    assert_eq!(v7.compare_to(&v8), std::cmp::Ordering::Less);
    assert_eq!(v8.compare_to(&v7), std::cmp::Ordering::Greater);
    assert_eq!(v9.compare_to(&v10), std::cmp::Ordering::Less);
    assert_eq!(v10.compare_to(&v9), std::cmp::Ordering::Greater);
    assert_eq!(v7.compare_to(&v7), std::cmp::Ordering::Equal);
    assert_eq!(v11.compare_to(&v12), std::cmp::Ordering::Less);
    assert_eq!(v11.compare_to(&v11), std::cmp::Ordering::Equal);
    assert_eq!(v12.compare_to(&v11), std::cmp::Ordering::Greater);
    assert_eq!(v13.compare_to(&v14), std::cmp::Ordering::Greater);
    assert_eq!(v15.compare_to(&v11), std::cmp::Ordering::Less);
    assert_eq!(v7.compare_to(&v16), std::cmp::Ordering::Less);
    assert_eq!(v16.compare_to(&v7), std::cmp::Ordering::Greater);
    assert_eq!(v7.compare_to(&v7), std::cmp::Ordering::Equal);
    assert_eq!(v16.compare_to(&v16), std::cmp::Ordering::Equal);
    assert_eq!(v16.compare_to(&v17), std::cmp::Ordering::Less);
    assert_eq!(v17.compare_to(&v16), std::cmp::Ordering::Greater);
    assert_eq!(v18.compare_to(&v19), std::cmp::Ordering::Less);
    assert_eq!(v18.compare_to(&v20), std::cmp::Ordering::Less);
    assert_eq!(v19.compare_to(&v18), std::cmp::Ordering::Greater);
    assert_eq!(v20.compare_to(&v18), std::cmp::Ordering::Greater);
    assert_eq!(v21.compare_to(&v1), std::cmp::Ordering::Equal);
}
