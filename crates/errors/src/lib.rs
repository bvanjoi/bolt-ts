use std::sync::Arc;

pub use miette;
use rts_span::{ModuleArena, ModuleID, ModulePath};
pub use thiserror;

#[derive(Debug)]
pub struct Diag {
    pub module_id: ModuleID,
    pub inner: Box<dyn miette::Diagnostic + Send + Sync + 'static>,
}

impl Diag {
    pub fn new(
        module_id: ModuleID,
        diag: Box<dyn miette::Diagnostic + Send + Sync + 'static>,
    ) -> Self {
        Self {
            module_id,
            inner: diag,
        }
    }

    pub fn emit_message(self, module_arena: &ModuleArena, no_color: bool) -> String {
        let source = module_arena.content_map.get(&self.module_id).unwrap();
        let filename = if let ModulePath::Real(filename) =
            module_arena.path_map.get(&self.module_id).unwrap()
        {
            Some(filename.display().to_string())
        } else {
            None
        };
        let source_code = SourceCode {
            filename,
            source: source.clone(),
        };

        let mut out = String::new();
        let mut error_report = miette::ErrReport::new_boxed(self.inner);
        error_report = error_report.with_source_code(source_code);
        let theme = if no_color {
            miette::GraphicalTheme::unicode_nocolor()
        } else {
            miette::GraphicalTheme::unicode()
        };
        miette::GraphicalReportHandler::new_themed(theme)
            .with_width(80)
            .render_report(&mut out, error_report.as_ref())
            .unwrap();
        out
    }
    
    pub fn emit(self, module_arena: &ModuleArena) {
        let no_color = match std::env::var("NO_COLOR") {
            Ok(string) => string != "0",
            _ => false,
        };
        let out = self.emit_message(module_arena, no_color);
        println!("{out}");
    }
}

use miette::MietteSpanContents;

#[derive(Debug)]
pub struct SourceCode {
    pub filename: Option<String>,
    pub source: Arc<String>,
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

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Default)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

pub fn offset_to_position(offset: usize, rope: &ropey::Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position { line, column })
}

pub fn miette_label_span_to_line_position(
    label: miette::LabeledSpan,
    source: &str,
) -> (Position, Position) {
    let rope = ropey::Rope::from_str(source);
    let start = offset_to_position(label.offset(), &rope).unwrap();
    let end = offset_to_position(label.len(), &rope).unwrap();
    (start, end)
}
