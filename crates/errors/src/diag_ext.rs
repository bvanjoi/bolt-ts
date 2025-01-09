use super::source_code::SourceCode;

use bolt_ts_span::{ModuleArena, ModuleID};

pub trait DiagnosticExt: miette::Diagnostic {
    fn module_id(&self) -> ModuleID;
    fn steal_related(&mut self) -> Option<Vec<Box<dyn DiagnosticExt + Send + Sync + 'static>>>;
}

#[derive(Debug)]
struct DiagConstructor {
    source_code: SourceCode,
    related: Option<Vec<Box<dyn miette::Diagnostic + Send + Sync + 'static>>>,
    inner: Box<dyn DiagnosticExt + Send + Sync + 'static>,
}

impl std::error::Error for DiagConstructor {}

impl std::fmt::Display for DiagConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl miette::Diagnostic for DiagConstructor {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.inner.code()
    }
    fn severity(&self) -> Option<miette::Severity> {
        self.inner.severity()
    }
    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.inner.help()
    }
    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.inner.url()
    }
    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.source_code)
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.inner.labels()
    }
    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn miette::Diagnostic> + 'a>> {
        use std::borrow::Borrow;
        self.related.as_ref().map(|related| {
            Box::new(
                related
                    .iter()
                    .map(|related| -> &(dyn miette::Diagnostic) { &*related.borrow() }),
            ) as Box<dyn Iterator<Item = &'a dyn miette::Diagnostic> + 'a>
        })
    }
    fn diagnostic_source(&self) -> Option<&dyn miette::Diagnostic> {
        self.inner.diagnostic_source()
    }
}

pub fn into_miette_diagnostic(
    mut diag: Box<dyn DiagnosticExt + Send + Sync + 'static>,
    module_arena: &ModuleArena,
) -> Box<dyn miette::Diagnostic + Send + Sync + 'static> {
    let module_id = diag.module_id();
    let source_code = SourceCode::new(module_arena, module_id);
    let related = diag.steal_related().map(|related| {
        related
            .into_iter()
            .map(|related| into_miette_diagnostic(related, module_arena))
            .collect::<Vec<_>>()
    });
    Box::new(DiagConstructor {
        source_code,
        related,
        inner: diag,
    })
}
