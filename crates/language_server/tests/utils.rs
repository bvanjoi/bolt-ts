use std::collections::HashMap;

use bolt_ts_compiler::add_default_libs_into_memory_files;
use bolt_ts_fs::MemoryFS;
use bolt_ts_language_server::LanguageService;
use bolt_ts_span::{ModuleID, Span};
use indexmap::IndexMap;

pub fn compile_single_input<'cx>(
    code: &str,
    parser_arena: &'cx bolt_ts_arena::bumpalo_herd::Herd,
    type_arena: &'cx bolt_ts_arena::bumpalo::Bump,
) -> TestState<'cx> {
    let json = serde_json::json!({
        "/index.ts": code
    });
    let files: indexmap::IndexMap<String, String> = serde_json::from_value(json).unwrap();
    parse_test_data(files, parser_arena, type_arena)
}

struct Range {
    lo: usize,
    hi: usize,
    filename: String,
    marker: Option<Marker>,
}

type MarkerArena = bolt_ts_arena::la_arena::Arena<Marker>;
type MarkerId = bolt_ts_arena::la_arena::Idx<Marker>;

fn parse_test_data<'cx>(
    files: indexmap::IndexMap<String, String>,
    parser_arena: &'cx bolt_ts_arena::bumpalo_herd::Herd,
    type_arena: &'cx bolt_ts_arena::bumpalo::Bump,
) -> TestState<'cx> {
    let mut marker_positions = HashMap::new();
    let mut markers = MarkerArena::new();
    let mut ranges = vec![];
    let files = files
        .into_iter()
        .map(|(path, content)| {
            let file = parse_file_content(
                &path,
                &content,
                &mut marker_positions,
                &mut markers,
                &mut ranges,
            );
            (path, file)
        })
        .collect::<IndexMap<_, _>>();
    assert!(files.len() == 1);
    TestState::new(
        files,
        0,
        ranges,
        marker_positions,
        markers,
        parser_arena,
        type_arena,
    )
}

fn parse_file_content(
    filename: &str,
    content: &str,
    marker_positions: &mut HashMap<String, MarkerId>,
    markers: &mut MarkerArena,
    ranges: &mut Vec<Range>,
) -> String {
    #[derive(Debug, PartialEq, Clone, Copy)]
    enum State {
        None,
        InSlashStarMarker(LocationInfo),
    }
    #[derive(Debug, PartialEq, Clone, Copy)]
    struct LocationInfo {
        position: usize,
        source_pos: usize,
        source_line: usize,
        source_column: usize,
    }
    #[derive(Debug, PartialEq)]
    struct RangeLocationInfo {
        position: usize,
        source_pos: usize,
        source_line: usize,
        source_column: usize,
        marker: Option<Marker>,
    }

    const VALID_MARKER_CHARS: &str =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$1234567890_";

    let content = content.as_bytes();
    let mut output = vec![];
    let mut open_ranges: Vec<RangeLocationInfo> = vec![];
    let mut local_ranges: Vec<Range> = vec![];
    let mut last_normal_char_pos = 0;
    let mut difference = 0;

    let mut state = State::None;

    let flush =
        |output: &mut Vec<u8>, last_normal_char_pos: usize, last_safe_char_index: Option<usize>| {
            let len = match last_safe_char_index {
                Some(last_safe_char_index) => last_safe_char_index - last_normal_char_pos,
                None => content.len() - last_normal_char_pos,
            };
            output.extend(&content[last_normal_char_pos..last_normal_char_pos + len]);
        };

    if content.is_empty() {
        return "".to_string();
    }

    let mut previous_char = content[0];
    let mut line = 1;
    let mut column = 1;
    for i in 1..content.len() {
        let current_char = content[i];

        match state {
            State::None => match (previous_char, current_char) {
                (b'[', b'|') => {
                    open_ranges.push(RangeLocationInfo {
                        position: i - 1 - difference,
                        source_pos: i - 1,
                        marker: None,
                        source_line: line,
                        source_column: column,
                    });
                    flush(&mut output, last_normal_char_pos, Some(i - 1));
                    last_normal_char_pos = i + 1;
                    difference += 2;
                }
                (b'|', b']') => {
                    let Some(range_start) = open_ranges.pop() else {
                        unreachable!(
                            "Unmatched range end marker at line {}, column {} in {}",
                            line, column, filename
                        );
                    };

                    let range = Range {
                        filename: filename.to_string(),
                        lo: range_start.position,
                        hi: i - 1 - difference,
                        marker: range_start.marker,
                    };
                    local_ranges.push(range);

                    flush(&mut output, last_normal_char_pos, Some(i - 1));
                    last_normal_char_pos = i + 1;
                    difference += 2;
                }
                (b'/', b'*') => {
                    state = State::InSlashStarMarker(LocationInfo {
                        position: i - 1 - difference,
                        source_pos: i - 1,
                        source_line: line,
                        source_column: column,
                    })
                }
                _ => {}
            },
            State::InSlashStarMarker(loc) => {
                if previous_char == b'*' && current_char == b'/' {
                    // record_marker
                    let marker_name_text =
                        String::from_utf8_lossy(&content[loc.source_pos + 2..i - 1]);
                    if marker_positions.contains_key(marker_name_text.as_ref()) {
                        panic!(
                            "Duplicate marker name '{}' at line {}, column {} in {}",
                            marker_name_text, loc.source_line, loc.source_column, filename
                        );
                    }
                    let marker = markers.alloc(Marker {
                        filename: filename.to_string(),
                        position: loc.position,
                        content: marker_name_text.to_string(),
                    });
                    marker_positions.insert(marker_name_text.to_string(), marker);
                    flush(&mut output, last_normal_char_pos, Some(loc.source_pos));
                    last_normal_char_pos = i + 1;
                    difference += i + 1 - loc.source_pos;
                    state = State::None;
                } else if !VALID_MARKER_CHARS.as_bytes().contains(&current_char) {
                    if current_char == b'*' && content.get(i + 1) == Some(&b'/') {
                        // nothing
                    } else {
                        flush(&mut output, last_normal_char_pos, Some(i));
                        last_normal_char_pos = i;
                        state = State::None;
                    }
                }
            }
        }

        if current_char == b'\n' && previous_char == b'\r' {
            continue;
        } else if current_char == b'\n' || current_char == b'\r' {
            line += 1;
            column = 1;
            continue;
        }

        column += 1;
        previous_char = current_char;
    }

    flush(&mut output, last_normal_char_pos, None);

    if !open_ranges.is_empty() {
        panic!(
            "Unmatched range start marker at line {}, column {} in {}",
            open_ranges[0].source_line, open_ranges[0].source_column, filename
        );
    }

    local_ranges.sort_by(|a, b| {
        if a.lo == b.lo {
            a.hi.cmp(&b.hi)
        } else {
            a.lo.cmp(&b.lo)
        }
    });
    ranges.extend(local_ranges);

    String::from_utf8(output).unwrap()
}

#[derive(Debug, PartialEq)]
struct Marker {
    filename: String,
    position: usize,
    content: String,
}

struct TestHost {
    fs: MemoryFS,
}

impl bolt_ts_language_server::LanguageServiceHost<MemoryFS> for TestHost {
    fn new(fs: MemoryFS) -> Self {
        Self { fs }
    }

    fn steal_fs(&mut self) -> MemoryFS {
        std::mem::take(&mut self.fs)
    }
}

pub struct TestState<'cx> {
    // TODO: delete
    input_files: indexmap::IndexMap<String, String>,
    ranges: Vec<Range>,
    markers: MarkerArena,
    marker_positions: HashMap<String, MarkerId>,
    active_file: usize,

    current_caret_pos: usize,
    selection_end: usize,
    language_service: LanguageService<'cx, MemoryFS, TestHost>,
    last_known_marker: Option<MarkerId>,
}

impl<'cx> TestState<'cx> {
    fn new(
        mut input_files: indexmap::IndexMap<String, String>,
        active_file: usize,
        ranges: Vec<Range>,
        marker_positions: HashMap<String, MarkerId>,
        markers: MarkerArena,
        parser_arena: &'cx bolt_ts_arena::bumpalo_herd::Herd,
        type_arena: &'cx bolt_ts_arena::bumpalo::Bump,
    ) -> Self {
        let mut atoms = bolt_ts_ast::keyword::init_atom_map();
        const DEFAULT_LIB_DIR: &str = "/node_modules/typescript/lib";
        add_default_libs_into_memory_files(&mut input_files, DEFAULT_LIB_DIR);
        let fs = MemoryFS::new(input_files.clone().into_iter(), &mut atoms).unwrap();
        let host = TestHost { fs };
        let language_service = LanguageService::<MemoryFS, TestHost>::new(
            host,
            atoms,
            parser_arena,
            type_arena,
            DEFAULT_LIB_DIR.to_string(),
        );
        Self {
            input_files: input_files.clone(),
            active_file,
            ranges,
            marker_positions,
            markers,
            current_caret_pos: 0,
            selection_end: usize::MAX,
            language_service,
            last_known_marker: None,
        }
    }

    fn get_marker_by_name(&self, name: &str) -> MarkerId {
        let Some(marker) = self.marker_positions.get(name).copied() else {
            panic!("Unknown marker '{}'", name);
        };
        marker
    }

    pub fn goto_marker(&mut self, marker: &str) {
        let (_, file) = self.input_files.get_index(self.active_file).unwrap();
        let marker_id = self.get_marker_by_name(marker);
        let marker = &self.markers[marker_id];
        if marker.position > file.len() {
            panic!(
                "Marker '{}' position {} is out of bounds for file of length {}",
                marker.content,
                marker.position,
                file.len(),
            );
        }
        self.last_known_marker = Some(marker_id);
        self.goto_position(marker.position);
    }

    fn goto_position(&mut self, pos: usize) {
        self.current_caret_pos = pos;
        self.selection_end = usize::MAX;
    }

    pub fn edit_insert(&mut self, text: &str) {
        let mut offset = self.current_caret_pos;

        let range = self.get_range();

        self.input_files[self.active_file].replace_range(range.0..range.1, "");

        for ch in text.chars() {
            self.current_caret_pos += 1;
            self.edit_script_and_update_markers(self.active_file, offset, offset, &ch.to_string());
            offset += 1;
        }
    }

    fn goto_defs_by_marker(&mut self, marker: &str, goto_defs: impl Fn(&mut Self) -> ()) {
        self.goto_marker(marker);
        goto_defs(self);
    }

    fn get_module_id_by_active_file(&self) -> ModuleID {
        let (file_name, _) = self.input_files.get_index(self.active_file).unwrap();
        let module_arena = self.language_service.compiler_result().module_arena();
        let path = std::path::Path::new(file_name);
        module_arena
            .modules()
            .iter()
            .find_map(|item| {
                let module_id = item.id();
                if module_arena.get_path(module_id).as_path() == path {
                    Some(module_id)
                } else {
                    None
                }
            })
            .unwrap()
    }

    pub fn verify_baseline_goto_implementation_by_marker(&mut self, marker: &str) -> Vec<String> {
        self.goto_marker(marker);
        let module_id = self.get_module_id_by_active_file();
        self.language_service
            .get_implementation_at_position(module_id, self.current_caret_pos)
            .into_iter()
            .map(|n| {
                let span = n.span();
                let content = self
                    .language_service
                    .compiler_result()
                    .module_arena()
                    .get_content(span.module());
                let content = content[span.lo() as usize..span.hi() as usize].to_string();
                content
            })
            .collect()
    }

    pub fn verify_current_file_content_is(&self, expected: &str) {
        let (_, file) = self.input_files.get_index(self.active_file).unwrap();
        assert_eq!(file, expected);
    }

    pub fn edit_script_and_update_markers(
        &mut self,
        file_index: usize,
        edit_start: usize,
        edit_end: usize,
        new_text: &str,
    ) {
        let file = &mut self.input_files[file_index];
        // TODO: edit_script
        let prefix = &file[..edit_start];
        let middle = new_text;
        let suffix = &file[edit_end..];
        *file = format!("{}{}{}", prefix, middle, suffix);

        let file_name = self.input_files.get_index(file_index).unwrap().0;
        for marker in self.markers.values_mut() {
            if !marker.filename.eq(file_name) {
                continue;
            }
            marker.position =
                Self::update_position(marker.position, edit_start, edit_end, new_text.len());
        }
    }

    pub fn get_range(&self) -> (usize, usize) {
        assert!(self.current_caret_pos <= self.selection_end);
        if self.selection_end == usize::MAX {
            (self.current_caret_pos, self.current_caret_pos)
        } else {
            (self.current_caret_pos, self.selection_end)
        }
    }

    pub fn update_position(pos: usize, edit_start: usize, edit_end: usize, len: usize) -> usize {
        if pos <= edit_start {
            pos
        } else if pos < edit_end {
            usize::MAX
        } else {
            pos + len - (edit_end - edit_start)
        }
    }
}

#[test]
#[should_panic]
fn panic_when_missing_range_location_0() {
    const CODE: &str = r#"
enum [|Foo {
  Foo1 = function initializer() { return 5 } (),
  Foo2 = 6
}
  "#;
    let parser_arena = bolt_ts_arena::bumpalo_herd::Herd::new();
    let type_arena = bolt_ts_arena::bumpalo::Bump::new();
    compile_single_input(CODE, &parser_arena, &type_arena);
}

#[test]
#[should_panic]
fn panic_when_missing_range_location_1() {
    const CODE: &str = r#"
enum Foo|] {
  Foo1 = function initializer() { return 5 } (),
  Foo2 = 6
}
  "#;
    let parser_arena = bolt_ts_arena::bumpalo_herd::Herd::new();
    let type_arena = bolt_ts_arena::bumpalo::Bump::new();
    compile_single_input(CODE, &parser_arena, &type_arena);
}

#[test]
#[should_panic]
fn panic_when_duplicate_marker_name() {
    const CODE: &str = r#"
/*marker*/*marker*/
  "#;
    let parser_arena = bolt_ts_arena::bumpalo_herd::Herd::new();
    let type_arena = bolt_ts_arena::bumpalo::Bump::new();
    compile_single_input(CODE, &parser_arena, &type_arena);
}

#[test]
#[should_panic]
fn panic_when_unknown_marker() {
    const CODE: &str = r#""#;
    let parser_arena = bolt_ts_arena::bumpalo_herd::Herd::new();
    let type_arena = bolt_ts_arena::bumpalo::Bump::new();
    let mut t = compile_single_input(CODE, &parser_arena, &type_arena);
    t.goto_marker("not_exist");
}
