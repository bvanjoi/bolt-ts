const CARRIAGE_RETURN: u8 = b'\r';
const LINE_FEED: u8 = b'\n';
const TAB: u8 = b'\t';
const VERTICAL_TAB: u8 = b'\x0B';
const FORM_FEED: u8 = b'\x0C';
const SPACE: u8 = b' ';
const SLASH: u8 = b'/';
const ASTERISK: u8 = b'*';

pub enum CommentKind {
    SingleLine,
    MultiLine,
}

pub fn iterate_comment_ranges<const REDUCE: bool, const TRAILING: bool>(
    text: &str,
    mut pos: usize,
    mut callback: impl FnMut(CommentKind, usize, usize, bool) -> bool,
) {
    let bytes = text.as_bytes();

    let mut pending_pos = usize::MAX;
    let mut pending_end = usize::MAX;
    let mut pending_kind = CommentKind::SingleLine;
    let mut collecting = TRAILING;
    let mut has_pending_comment_range = false;
    let mut pending_has_trailing_newline = false;
    if pos == 0 {
        collecting = true;
        // TODO: shebang
    }
    while let Some(ch) = bytes.get(pos).copied() {
        match ch {
            CARRIAGE_RETURN => {
                if bytes.get(pos + 1) == Some(&LINE_FEED) {
                    pos += 1;
                }
                pos += 1;
                if TRAILING {
                    break;
                }
                if has_pending_comment_range {
                    pending_has_trailing_newline = true;
                }
            }
            LINE_FEED => {
                pos += 1;
                if TRAILING {
                    break;
                }
                if has_pending_comment_range {
                    pending_has_trailing_newline = true;
                }
            }
            TAB | VERTICAL_TAB | FORM_FEED | SPACE => {
                pos += 1;
            }
            SLASH => {
                let Some(next_char) = bytes.get(pos + 1).copied() else {
                    break;
                };
                let mut has_trailing_new_line = false;
                let next_is_slash = next_char == SLASH;
                if next_is_slash || next_char == ASTERISK {
                    let kind = if next_is_slash {
                        CommentKind::SingleLine
                    } else {
                        CommentKind::MultiLine
                    };
                    let start_pos = pos;
                    pos += 2;
                    if next_is_slash {
                        while let Some(ch) = bytes.get(pos).copied() {
                            if super::is_line_break(ch) {
                                has_trailing_new_line = true;
                                break;
                            }
                            pos += 1;
                        }
                    } else {
                        while let Some(ch) = bytes.get(pos).copied() {
                            if ch == ASTERISK && bytes.get(pos + 1) == Some(&SLASH) {
                                pos += 2;
                                break;
                            }
                            pos += 1;
                        }
                    }
                    if collecting {
                        if has_pending_comment_range {
                            debug_assert!(pending_pos != usize::MAX);
                            debug_assert!(pending_end != usize::MAX);
                            let stop = callback(
                                pending_kind,
                                pending_pos,
                                pending_end,
                                pending_has_trailing_newline,
                            );
                            if !REDUCE && stop {
                                break;
                            }
                        }
                        pending_pos = start_pos;
                        pending_end = pos;
                        pending_kind = kind;
                        pending_has_trailing_newline = has_trailing_new_line;
                        has_pending_comment_range = true;
                    }
                    continue;
                }
                break;
            }
            _ => {
                // TODO: whitespace
                break;
            }
        }
    }
}
