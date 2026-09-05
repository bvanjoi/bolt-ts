#[derive(Debug, Default, Clone)]
pub struct CompilerOptions(serde_json::Map<String, serde_json::Value>);

impl std::ops::Index<&'static str> for CompilerOptions {
    type Output = serde_json::Value;

    fn index(&self, index: &'static str) -> &Self::Output {
        self.0.get(index).expect("Key not found")
    }
}

impl CompilerOptions {
    #[allow(dead_code)]
    pub(super) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[allow(dead_code)]
    pub(super) fn len(&self) -> usize {
        self.0.len()
    }

    pub(super) fn extend<T: IntoIterator<Item = (String, serde_json::Value)>>(&mut self, iter: T) {
        self.0.extend(iter)
    }

    pub fn as_serde_json(&self) -> &serde_json::Map<String, serde_json::Value> {
        &self.0
    }
}
