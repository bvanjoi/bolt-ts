use bolt_ts_ast::keyword;
use bolt_ts_atom::Atom;

use crate::FileReference;

use super::{PResult, scan::is_line_break};

enum State {
    ExpectEq,
    ExpectString,
}

impl super::ParserState<'_, '_> {
    pub(super) fn scan_triple_slash_xml_pragma(&mut self) {
        assert_eq!(self.ch_unchecked(), b'<');
        self.pos += 1;
        if self.ch() == Some(b'r') {
            self.scan_reference_pragma();
        } else {
            self.pos += 1
        }
    }

    fn scan_reference_pragma(&mut self) {
        assert_eq!(self.ch_unchecked(), b'r');
        const REFERENCE_PARAM_PREFIX: &[u8] = b"reference ";
        if !self.next_content_is(REFERENCE_PARAM_PREFIX) {
            return;
        }

        while self.pos < self.end() && !is_line_break(self.ch_unchecked()) {
            let ch = self.ch_unchecked();
            if self.next_content_is(b"/>") {
                // finished
                break;
            } else if self.next_content_is(b"lib") {
                if let Ok(v) = self.scan_reference_lib_pragma() {
                    self.lib_reference_directives
                        .push(FileReference { filename: v });
                } else {
                    self.pos += 1;
                    return;
                }
            } else if self.next_content_is(b"no-default-lib") {
                if let Ok(v) = self.scan_reference_lib_pragma() {
                    if v != keyword::KW_TRUE {
                        // TODO: error
                    } else {
                        self.has_no_default_lib = true;
                    }
                } else {
                    self.pos += 1;
                    return;
                }
            } else if ch.is_ascii_whitespace() {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn scan_reference_lib_pragma(&mut self) -> PResult<Atom> {
        let mut state = State::ExpectEq;
        while self.pos < self.end() && !is_line_break(self.ch_unchecked()) {
            let ch = self.ch_unchecked();
            match state {
                State::ExpectEq => {
                    if ch == b'=' {
                        state = State::ExpectString;
                        self.pos += 1;
                    } else if ch.is_ascii_whitespace() {
                        continue;
                    } else {
                        return Err(());
                    }
                }
                State::ExpectString => {
                    if ch == b'"' || ch == b'\'' {
                        let (value, _) = self.scan_string(ch, false);
                        let s = unsafe { std::str::from_utf8_unchecked(&value) };
                        let atom = self.atoms.lock().unwrap().atom(s);
                        return Ok(atom);
                    } else if ch.is_ascii_whitespace() {
                        continue;
                    } else {
                        return Err(());
                    }
                }
            }
        }
        Err(())
    }
}
