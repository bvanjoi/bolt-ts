use bolt_ts_span::{ModuleArena, ModuleID, ModulePath};
use miette::MietteSpanContents;

#[derive(Debug)]
pub(crate) struct SourceCode {
    pub filename: Option<String>,
    pub source: std::sync::Arc<String>,
}

impl SourceCode {
    pub fn new(module_arena: &ModuleArena, module_id: ModuleID) -> Self {
        let source = module_arena.get_content(module_id);
        let filename = if let ModulePath::Real(filename) = module_arena.get_path(module_id) {
            let cwd = std::env::current_dir().unwrap();
            let relative =
                relative_path::PathExt::relative_to(filename.as_path(), cwd.as_path()).unwrap();
            Some(relative.to_string())
        } else {
            None
        };
        Self {
            filename,
            source: source.clone(),
        }
    }
}

impl miette::SourceCode for SourceCode {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        let inner_contents =
            self.source
                .read_span(span, context_lines_before, context_lines_after)?;
        let contents = match &self.filename {
            Some(filename) => MietteSpanContents::new_named(
                filename.to_string(),
                inner_contents.data(),
                *inner_contents.span(),
                inner_contents.line(),
                inner_contents.column(),
                inner_contents.line_count(),
            ),
            None => MietteSpanContents::new(
                inner_contents.data(),
                *inner_contents.span(),
                inner_contents.line(),
                inner_contents.column(),
                inner_contents.line_count(),
            ),
        };
        Ok(Box::new(contents))
    }
}
