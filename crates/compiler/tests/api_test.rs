use indexmap::IndexMap;

mod api {
    mod white_space_trimming;
}

fn compile_single_input(code: &str) -> db::DB {
    let json = serde_json::json!({
        "/index.ts": code
    });
    let files: indexmap::IndexMap<String, String> = serde_json::from_value(json).unwrap();
    let files = files
        .into_iter()
        .map(|(path, content)| {
            let file = db::parse_file_content(&path, &content);
            (path, file)
        })
        .collect::<IndexMap<_, _>>();
    assert!(files.len() == 1);
    db::DB::new(files, 0)
}

mod db {

    pub fn parse_file_content(filename: &str, content: &str) -> self::File {
        #[derive(Debug, PartialEq, Clone, Copy)]
        enum State {
            None,
            InSlashStarMarker(LocationInfo),
        }
        #[derive(Debug, PartialEq, Clone, Copy)]
        struct LocationInfo {
            position: usize,
            source_pos: usize,
        }

        const VALID_MARKER_CHARS: &str =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$1234567890_";

        let content = content.as_bytes();
        let mut markers = vec![];
        let mut output = vec![];
        let mut last_normal_char_pos = 0;
        let mut difference = 0;

        let mut state = State::None;

        let flush = |output: &mut Vec<u8>,
                     last_normal_char_pos: usize,
                     last_safe_char_index: Option<usize>| {
            let len = match last_safe_char_index {
                Some(last_safe_char_index) => last_safe_char_index - last_normal_char_pos,
                None => content.len() - last_normal_char_pos,
            };
            output.extend(&content[last_normal_char_pos..last_normal_char_pos + len]);
        };

        if content.is_empty() {
            return self::File {
                content: "".to_string(),
                markers,
            };
        }

        let mut previous_char = content[0];
        for i in 1..content.len() {
            let current_char = content[i];

            match state {
                State::None => {
                    if previous_char == b'/' && current_char == b'*' {
                        state = State::InSlashStarMarker(LocationInfo {
                            position: i - 1 - difference,
                            source_pos: i - 1,
                        });
                    }
                }
                State::InSlashStarMarker(loc) => {
                    if previous_char == b'*' && current_char == b'/' {
                        markers.push(Marker {
                            offset: loc.position,
                            content: String::from_utf8_lossy(&content[loc.source_pos + 2..i - 1])
                                .trim()
                                .to_string(),
                        });
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
                continue;
            }

            previous_char = current_char;
        }

        flush(&mut output, last_normal_char_pos, None);

        File {
            content: String::from_utf8(output).unwrap(),
            markers,
        }
    }

    pub struct Marker {
        offset: usize,
        content: String,
    }

    pub struct File {
        pub content: String,
        pub markers: Vec<Marker>,
    }

    pub struct DB {
        files: indexmap::IndexMap<String, File>,
        active_file: usize,

        current_caret_pos: usize,
        selection_end: usize,
    }

    impl DB {
        pub fn new(files: indexmap::IndexMap<String, File>, active_file: usize) -> Self {
            Self {
                files,
                active_file,
                current_caret_pos: 0,
                selection_end: usize::MAX,
            }
        }

        pub fn goto_marker(&mut self, marker: &str) {
            let (_, file) = self.files.get_index(self.active_file).unwrap();
            let Some(marker) = file.markers.iter().find(|m| m.content == marker) else {
                return;
            };
            self.current_caret_pos = marker.offset;
            self.selection_end = usize::MAX;
        }

        pub fn edit_insert(&mut self, text: &str) {
            let mut offset = self.current_caret_pos;

            let range = self.get_range();

            self.files[self.active_file]
                .content
                .replace_range(range.0..range.1, "");

            for ch in text.chars() {
                self.current_caret_pos += 1;

                self.edit_script_and_update_markers(
                    self.active_file,
                    offset,
                    offset,
                    &ch.to_string(),
                );
                offset += 1;
            }
        }

        pub fn verify_current_file_content_is(&self, expected: &str) {
            let (_, file) = self.files.get_index(self.active_file).unwrap();
            assert_eq!(file.content, expected);
        }

        fn edit_script_and_update_markers(
            &mut self,
            file_index: usize,
            edit_start: usize,
            edit_end: usize,
            new_text: &str,
        ) {
            let file = &mut self.files[file_index];
            // TODO: edit_script
            let prefix = &file.content[..edit_start];
            let middle = new_text;
            let suffix = &file.content[edit_end..];
            file.content = format!("{}{}{}", prefix, middle, suffix);

            for marker in &mut file.markers {
                marker.offset =
                    Self::update_position(marker.offset, edit_start, edit_end, new_text.len());
            }
        }

        fn get_range(&self) -> (usize, usize) {
            assert!(self.current_caret_pos <= self.selection_end);
            if self.selection_end == usize::MAX {
                (self.current_caret_pos, self.current_caret_pos)
            } else {
                (self.current_caret_pos, self.selection_end)
            }
        }

        fn update_position(pos: usize, edit_start: usize, edit_end: usize, len: usize) -> usize {
            if pos <= edit_start {
                pos
            } else if pos < edit_end {
                usize::MAX
            } else {
                pos + len - (edit_end - edit_start)
            }
        }
    }
}
