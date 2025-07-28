use bolt_ts_atom::AtomId;

#[derive(Clone, Copy, Debug)]
pub enum PragmaCommentKind {
    /// `/// <pragma-name arg_name="value" />`
    TripleSlashXML,
    /// `// @pragma-name arg_val1 arg_val2`
    SingleLine,
    /// ```text
    /// /**
    /// * @pragma-name arg_val1 arg_val2
    /// */
    /// ```
    MultiLine,
}

#[derive(Debug)]
pub enum PragmaSpec {
    /// `<reference xxxx />`
    Reference(ReferencePragma),
    /// `<amd-dependency xxxx />`
    AmdDependency(AmdDependencyPragma),
    /// `<amd-module xxxx />`
    AmdModule(AmdModulePragma),
}

impl PragmaSpec {
    fn comment_kind(&self) -> PragmaCommentKind {
        match self {
            PragmaSpec::AmdModule(_) | PragmaSpec::AmdDependency(_) | PragmaSpec::Reference(_) => {
                PragmaCommentKind::TripleSlashXML
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ReferencePragma {
    Types {
        optional: bool,
        capture_span: bool,
    },
    Lib {
        optional: bool,
        capture_span: bool,
        value: AtomId,
    },
    Path {
        optional: bool,
        capture_span: bool,
    },
    NoDefaultLib {
        optional: bool,
    },
    ResolutionMode {
        optional: bool,
    },
    Preserve {
        optional: bool,
    },
}

#[derive(Debug)]
pub struct AmdDependencyPragma {
    args: Vec<AmdDependencyPragmaArg>,
}

#[derive(Debug, Clone, Copy)]
pub enum AmdDependencyPragmaArg {
    Path,
    Name { optional: bool },
}

#[derive(Debug)]
pub struct AmdModulePragma {
    /// name -> name
    args: Vec<()>,
}

#[derive(Debug, Default)]
pub struct PragmaMap(nohash_hasher::IntMap<AtomId, PragmaSpec>);
