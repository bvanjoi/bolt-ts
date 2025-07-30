use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_parser::ParseResultForGraph;
use bolt_ts_span::Span;
use bolt_ts_utils::no_hashset_with_capacity;

use bolt_ts_parser::CommentDirective;

pub(super) fn get_merged_diags(
    diags: Vec<bolt_ts_errors::Diag>,
    p: &super::Parser,
    ms: &super::ModuleArena,
) -> Vec<bolt_ts_errors::Diag> {
    // get_diags_with_preceding_directives
    let mut result = Vec::with_capacity(diags.len());

    let mut directives = CommentDirectivesMap::new(p);

    for diag in diags {
        let module_id = diag.inner.module_id();
        let p_r = p.get(module_id);
        if p_r.comment_directives.is_empty() {
            result.push(diag);
        } else if let Some(primary_label) = diag.inner.as_ref().labels().map(|labels| {
            labels
                .into_iter()
                .find(|label| label.primary())
                .expect("at least one primary label")
        }) {
            // diag
            let input = ms.get_content(module_id).as_bytes();
            if !mark_preceding_comment_directive_line(
                primary_label.offset(),
                &mut directives,
                p_r,
                input,
                module_id,
            ) {
                result.push(diag);
            }
        } else {
            result.push(diag);
        }
    }

    // get unused_expectations
    let errors = directives.get_unused_expectations().into_iter().map(|m| {
        let error = UnusedTsExpectErrorDirective { span: m.range };
        bolt_ts_errors::Diag {
            inner: Box::new(error),
        }
    });
    result.extend(errors);
    result
}

fn mark_preceding_comment_directive_line<'cx>(
    diag_start: usize,
    directives: &mut CommentDirectivesMap<'_, 'cx>,
    file: &ParseResultForGraph<'cx>,
    input: &[u8],
    module_id: bolt_ts_span::ModuleID,
) -> bool {
    let diag_pos = compute_line_and_char_of_pos(&file.line_map, diag_start);
    if diag_pos.line == 0 {
        return false;
    }
    let mut directive_line = diag_pos.line - 1;
    loop {
        if directives.mark_used(module_id, directive_line) {
            // marked
            return true;
        }
        if directive_line == 0 {
            break;
        }
        let line_start = file.line_map[directive_line] as usize;
        let line_end = file.line_map[directive_line + 1] as usize;
        let line_text = input[line_start..line_end].trim_ascii();
        if !line_text.is_empty() && !line_text.starts_with(b"//") {
            return false;
        }

        directive_line -= 1;
    }
    false
}

struct CommentDirectivesMap<'p, 'cx> {
    p: &'p super::Parser<'cx>,
    all: Vec<nohash_hasher::IntMap<u32, CommentDirective>>,
    used: Vec<nohash_hasher::IntSet<u32>>,
}

impl<'p, 'cx> CommentDirectivesMap<'p, 'cx> {
    fn new(p: &'p super::Parser<'cx>) -> Self {
        let used = p
            .map
            .iter()
            .map(|r| no_hashset_with_capacity(r.comment_directives.len()))
            .collect();
        let all = p
            .map
            .iter()
            .map(|r| r.comment_directives.iter().map(|c| (c.line, *c)).collect())
            .collect();
        Self { p, used, all }
    }

    fn mark_used(&mut self, module_id: bolt_ts_span::ModuleID, line: usize) -> bool {
        let line = line as u32;
        let used = &mut self.used[module_id.as_usize()];
        if used.contains(&line) {
            true
        } else if self.all[module_id.as_usize()].contains_key(&line) {
            used.insert(line);
            true
        } else {
            false
        }
    }

    fn get_unused_expectations(&self) -> Vec<CommentDirective> {
        self.all
            .iter()
            .enumerate()
            .flat_map(|(idx, all)| {
                let used = &self.used[idx];
                all.iter().filter_map(|(line, directive)| {
                    if matches!(
                        directive.kind,
                        bolt_ts_parser::CommentDirectiveKind::ExpectError
                    ) && !used.contains(line)
                    {
                        Some(*directive)
                    } else {
                        None
                    }
                })
            })
            .collect::<Vec<_>>()
    }
}

fn compute_line_and_char_of_pos(line_starts: &[u32], position: usize) -> bolt_ts_errors::Position {
    let line = compute_line_of_pos(line_starts, position);
    bolt_ts_errors::Position {
        line,
        column: position - (line_starts[line] as usize),
    }
}

fn compute_line_of_pos(line_starts: &[u32], position: usize) -> usize {
    debug_assert!(line_starts.is_sorted());
    let position = position as u32;
    match line_starts.binary_search(&position) {
        Ok(line) => line,
        Err(line) => line - 1,
    }
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Unused '@ts-expect-error' directive.")]
pub(crate) struct UnusedTsExpectErrorDirective {
    #[label(primary)]
    pub span: Span,
}
