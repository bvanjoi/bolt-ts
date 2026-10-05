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
#[derive(Debug, Default)]
pub struct LeadingTrailingComments {
    comments: IndexMap<u32, CommentsAtToken>,
}

impl LeadingTrailingComments {
    // fn is_pos_incremental(&self, pos: u32) -> bool {
    //     if let Some((&last_pos, _)) = self.comments.last() {
    //         last_pos <= pos
    //     } else {
    //         true
    //     }
    // }

    pub fn add_leading_comment(&mut self, pos: u32, comment: Comment) {
        //TODO: debug_assert!(self.is_pos_incremental(pos));
        let comments = self.comments.entry(pos).or_default();
        // TODO: debug_assert!(!comments.leading.contains(&comment));
        comments.leading.push(comment);
    }

    pub fn get_leading_comments(&self, start: u32) -> Option<&[Comment]> {
        self.comments.get(&start).map(|v| v.leading.as_slice())
    }

    pub fn add_trailing_comment(&mut self, pos: u32, comment: Comment) {
        //TODO: debug_assert!(self.is_pos_incremental(pos));
        let comments = self.comments.entry(pos).or_default();
        // TODO: debug_assert!(!comments.trailing.contains(&comment));
        comments.trailing.push(comment);
    }
}
