pub mod diag_ext;
mod source_code;

pub use bolt_ts_diag_derive::DiagnosticExt;
use bolt_ts_span::ModuleArena;
use diag_ext::into_miette_diagnostic;
pub use miette;
pub use thiserror;

#[derive(Debug)]
pub struct Diag {
    pub inner: Box<dyn diag_ext::DiagnosticExt + Send + Sync + 'static>,
}

impl Diag {
    pub fn emit_message(self, module_arena: &ModuleArena, no_color: bool) -> String {
        let mut out = String::new();
        let error = into_miette_diagnostic(self.inner, module_arena);
        let error_report = miette::ErrReport::new_boxed(error);
        let theme = if no_color {
            miette::GraphicalTheme::unicode_nocolor()
        } else {
            miette::GraphicalTheme::unicode()
        };
        miette::GraphicalReportHandler::new_themed(theme)
            .with_width(80)
            .with_context_lines(0)
            .render_report(&mut out, error_report.as_ref())
            .unwrap();
        out.trim_start_matches('\n').to_string()
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

// TODO: use parser.line_map instead of ropey::Rope
pub fn miette_label_span_to_line_position(
    label: miette::LabeledSpan,
    source: &str,
) -> (Position, Position) {
    let rope = ropey::Rope::from_str(source);
    let start = offset_to_position(label.offset(), &rope).unwrap();
    let end = offset_to_position(label.len(), &rope).unwrap();
    (start, end)
}
