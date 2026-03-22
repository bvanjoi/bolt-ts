pub trait GenerateGithubYaml {
    fn to_yaml(&self) -> serde_yaml::Value;
}
