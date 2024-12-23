#[derive(Debug, Clone, Default)]
pub enum OutDir {
    #[default]
    OwnRoot,
    Custom(String),
}
