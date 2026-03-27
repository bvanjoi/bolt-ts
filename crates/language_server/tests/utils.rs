use std::collections::HashMap;

use bolt_ts_language_server::LanguageService;
use indexmap::IndexMap;
use tower_lsp::LanguageServer;

pub fn compile_single_input(code: &str) -> TestState {
    let json = serde_json::json!({
        "/index.ts": code
    });
    let files: indexmap::IndexMap<String, String> = serde_json::from_value(json).unwrap();
    parse_test_data(files)
}
struct Range {
    lo: usize,
    hi: usize,
    marker: Option<Marker>,
}

type MarkerArena = bolt_ts_arena::la_arena::Arena<Marker>;
type MarkerId = bolt_ts_arena::la_arena::Idx<Marker>;

fn parse_test_data(files: indexmap::IndexMap<String, String>) -> TestState {
    let mut marker_positions = HashMap::new();
    let mut markers = MarkerArena::new();
    // let mut ranges = vec![];
    let files = files
        .into_iter()
        .map(|(path, content)| {
            let file = parse_file_content(&path, &content, &mut marker_positions, &mut markers);
            (path, file)
        })
        .collect::<IndexMap<_, _>>();
    assert!(files.len() == 1);
    TestState::new(files, 0, marker_positions, markers)
}

fn parse_file_content(
    filename: &str,
    content: &str,
    marker_positions: &mut HashMap<String, MarkerId>,
    markers: &mut MarkerArena,
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
                        offset: loc.position,
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

    String::from_utf8(output).unwrap()
}

#[derive(Debug, PartialEq)]
struct Marker {
    filename: String,
    offset: usize,
    content: String,
}

pub struct TestState {
    input_files: indexmap::IndexMap<String, String>,
    markers: MarkerArena,
    marker_positions: HashMap<String, MarkerId>,
    active_file: usize,

    current_caret_pos: usize,
    selection_end: usize,
    language_service: LanguageService,
}

impl TestState {
    fn new(
        input_files: indexmap::IndexMap<String, String>,
        active_file: usize,
        marker_positions: HashMap<String, MarkerId>,
        markers: MarkerArena,
    ) -> Self {
        Self {
            input_files,
            active_file,
            marker_positions,
            markers,
            current_caret_pos: 0,
            selection_end: usize::MAX,
            language_service: LanguageService::new(),
        }
    }

    pub fn goto_marker(&mut self, marker: &str) {
        let (_, file) = self.input_files.get_index(self.active_file).unwrap();
        let Some(marker) = self.marker_positions.get(marker).copied() else {
            return;
        };
        self.current_caret_pos = self.markers[marker].offset;
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
            marker.offset =
                Self::update_position(marker.offset, edit_start, edit_end, new_text.len());
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
    compile_single_input(CODE);
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
    compile_single_input(CODE);
}

#[test]
#[should_panic]
fn panic_when_duplicate_marker_name() {
    const CODE: &str = r#"
/*marker*/*marker*/
  "#;
    compile_single_input(CODE);
}
