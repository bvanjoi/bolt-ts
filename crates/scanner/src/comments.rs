use indexmap::IndexMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentKind {
    SingleLine,
    MultiLine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Comment {
    start: u32,
    end: u32,
    kind: CommentKind,
}

impl Comment {
    pub fn new(start: u32, end: u32, kind: CommentKind) -> Self {
        Self { start, end, kind }
    }

    pub fn start(&self) -> u32 {
        self.start
    }
    pub fn end(&self) -> u32 {
        self.end
    }
    pub fn kind(&self) -> CommentKind {
        self.kind
    }
}

#[derive(Debug, Default)]
pub struct CommentsAtToken {
    leading: Vec<Comment>,
    trailing: Vec<Comment>,
}

impl CommentsAtToken {
    pub fn get_leading_comments(&self) -> &[Comment] {
        &self.leading
    }

    pub fn get_trailing_comments(&self) -> &[Comment] {
        &self.trailing
    }
}

#[derive(Debug, Default)]
pub struct LeadingTrailingComments {
    comments: IndexMap<u32, Option<CommentsAtToken>>,
}

impl LeadingTrailingComments {
    #[track_caller]
    fn ensure_pos_incremental(&self, pos: u32) {
        if let Some((&last_pos, _)) = self.comments.last() {
            debug_assert!(last_pos <= pos, "pos: {pos}, last_pos: {last_pos}");
        }
    }

    #[track_caller]
    pub fn mark_no_comments(&mut self, pos: u32) {
        self.ensure_pos_incremental(pos);
        self.comments.entry(pos).or_insert(None);
    }

    #[track_caller]
    pub fn add_leading_comment(&mut self, pos: u32, comment: Comment) {
        self.ensure_pos_incremental(pos);
        match self.comments.entry(pos) {
            indexmap::map::Entry::Occupied(occ) => {
                let Some(occ) = occ.into_mut() else {
                    // Trying to add leading comment at pos {pos} where no comments are allowed
                    unreachable!();
                };
                debug_assert!(!occ.leading.contains(&comment));
                occ.leading.push(comment);
            }
            indexmap::map::Entry::Vacant(vac) => {
                let mut comments_at_token = CommentsAtToken::default();
                comments_at_token.leading.push(comment);
                vac.insert(Some(comments_at_token));
            }
        }
    }

    pub fn get_comments_by_index(&self, index: usize) -> Option<(&u32, &Option<CommentsAtToken>)> {
        self.comments.get_index(index)
    }

    #[track_caller]
    pub fn add_trailing_comment(&mut self, pos: u32, comment: Comment) {
        self.ensure_pos_incremental(pos);
        match self.comments.entry(pos) {
            indexmap::map::Entry::Occupied(occ) => {
                let Some(occ) = occ.into_mut() else {
                    // Trying to add trailing comment at pos {pos} where no comments are allowed
                    unreachable!();
                };
                debug_assert!(!occ.trailing.contains(&comment));
                occ.trailing.push(comment);
            }
            indexmap::map::Entry::Vacant(vac) => {
                let mut comments_at_token = CommentsAtToken::default();
                comments_at_token.trailing.push(comment);
                vac.insert(Some(comments_at_token));
            }
        }
    }
}
