use indexmap::IndexMap;

mod api {
    mod white_space_trimming;
}

fn compile_single_input(code: &str) -> DB {
    let json = serde_json::json!({
        "/index.ts": code
    });
    let files: indexmap::IndexMap<String, String> = serde_json::from_value(json).unwrap();
    let files = files
        .into_iter()
        .map(|(path, content)| {
            let markers = parse_file_content(&content);
            (path, File { content, markers })
        })
        .collect::<IndexMap<_, _>>();
    assert!(files.len() == 1);
    DB {
        files,
        active_file: 0,
        pos: Pos { start: 0, end: 0 },
    }
}

fn parse_file_content(content: &str) -> Vec<Marker> {
    let mut pos = 0;
    let content = content.as_bytes();
    let mut markers = vec![];
    while let Some(c) = content.get(pos) {
        match *c {
            b'/' if content.get(pos + 1) == Some(&b'*') => {
                let mut j = pos + 2;
                while let Some(&ch) = content.get(j) {
                    if ch == b'*' && content.get(j + 1) == Some(&b'/') {
                        let marker = Marker {
                            pos,
                            content: String::from_utf8_lossy(&content[pos + 2..j]).to_string(),
                        };
                        markers.push(marker);
                        pos = j + 2;
                        break;
                    }
                    j += 1;
                }
            }
            _ => {
                pos += 1;
            }
        }
    }
    markers
}

struct Marker {
    pos: usize,
    content: String,
}

struct File {
    content: String,
    markers: Vec<Marker>,
}

#[derive(Debug, Clone, Copy)]
struct Pos {
    start: usize,
    end: usize,
}

struct DB {
    files: indexmap::IndexMap<String, File>,
    active_file: usize,
    pos: Pos,
}

impl DB {
    fn goto_marker(&mut self, marker: &str) {
        let (_, file) = self.files.get_index(self.active_file).unwrap();
        let Some(marker) = file.markers.iter().find(|m| m.content == marker) else {
            return;
        };
        self.pos = Pos {
            start: marker.pos,
            end: marker.pos + marker.content.len() + 4, // 4 means `/*`.len() + `*/`.len(),
        };
    }

    fn edit_insert(&mut self, input: &str) {
        let (_, file) = self.files.get_index_mut(self.active_file).unwrap();
        let len = self.pos.end - self.pos.start;
        for _ in 0..len {
            file.content.remove(self.pos.start);
        }
        file.content.insert_str(self.pos.start, input);
    }

    fn verify_current_file_content_is(&self, expected: &str) {
        let (_, file) = self.files.get_index(self.active_file).unwrap();
        assert_eq!(file.content, expected);
    }
}
