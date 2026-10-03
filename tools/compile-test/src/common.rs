#[derive(Clone, Copy, PartialEq, Debug, Hash)]
pub enum PassMode {
    Check,
    Build,
    Run,
}

#[derive(Clone, Copy, PartialEq, Debug, Hash)]
pub enum FailMode {
    Check,
    Build,
    Run,
}
