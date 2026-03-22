use super::define_generate_yaml::GenerateGithubYaml;

use serde_yaml::{Mapping, Value};

pub enum Step {
    Run(Box<dyn RunStep>),
    Uses(Box<dyn UseStep>),
}

impl GenerateGithubYaml for Step {
    fn to_yaml(&self) -> Value {
        match self {
            Step::Run(run) => run.as_ref().to_yaml(),
            Step::Uses(uses) => uses.as_ref().to_yaml(),
        }
    }
}

pub trait RunStep: std::fmt::Debug {
    fn name(&self) -> &str;
    fn commands(&self) -> Vec<String>;
    fn working_directory(&self) -> Option<&str> {
        None
    }
}

impl GenerateGithubYaml for &dyn RunStep {
    fn to_yaml(&self) -> Value {
        let mut map = Mapping::new();
        map.insert(
            Value::String("name".to_string()),
            Value::String(self.name().to_string()),
        );
        if let Some(working_directory) = self.working_directory() {
            map.insert(
                Value::String("working-directory".to_string()),
                Value::String(working_directory.to_string()),
            );
        }
        let commands = self.commands();
        if commands.is_empty() {
            return Value::Mapping(map);
        }
        map.insert(
            Value::String("run".to_string()),
            Value::String(commands.join("\n")),
        );
        Value::Mapping(map)
    }
}

pub trait UseStep: std::fmt::Debug {
    fn name(&self) -> &str;
    fn uses_action_name(&self) -> &str;
    fn uses_action_version(&self) -> Option<&str>;
    fn with(&self) -> Option<Vec<(String, serde_yaml::Value)>>;
}

impl GenerateGithubYaml for &dyn UseStep {
    fn to_yaml(&self) -> Value {
        let mut map = Mapping::new();
        map.insert(
            Value::String("name".to_string()),
            Value::String(self.name().to_string()),
        );
        let mut action = self.uses_action_name().to_string();
        if let Some(version) = self.uses_action_version() {
            action.push('@');
            action.push_str(version);
        }
        map.insert(Value::String("uses".to_string()), Value::String(action));
        if let Some(with) = self.with() {
            map.insert(
                Value::String("with".to_string()),
                Value::Mapping(Mapping::from_iter(
                    with.iter()
                        .map(|(k, v)| (Value::String(k.clone()), v.clone())),
                )),
            );
        }
        Value::Mapping(map)
    }
}
