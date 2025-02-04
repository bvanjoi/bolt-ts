#[derive(Debug, Clone, Default, PartialEq)]
pub enum OutDir {
    #[default]
    OwnRoot,
    Custom(String),
}
