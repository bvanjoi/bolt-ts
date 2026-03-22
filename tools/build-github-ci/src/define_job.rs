use super::define_generate_yaml::GenerateGithubYaml;
use super::define_step::Step;

use serde_yaml::{Mapping, Value};

#[derive(Debug, Clone, Copy)]
pub enum RunOn {
    UbuntuLatest,
}

impl RunOn {
    pub fn as_str(&self) -> &str {
        match self {
            RunOn::UbuntuLatest => "ubuntu-latest",
        }
    }
}

impl GenerateGithubYaml for RunOn {
    fn to_yaml(&self) -> Value {
        match self {
            RunOn::UbuntuLatest => Value::String("ubuntu-latest".to_string()),
        }
    }
}

pub trait Job {
    fn job_id(&self) -> &str;
    fn runs_on(&self) -> Vec<RunOn>;
    fn steps(&self) -> Vec<Step>;
    fn needs(&self) -> Option<Vec<&str>> {
        None
    }
}

impl GenerateGithubYaml for &dyn Job {
    fn to_yaml(&self) -> Value {
        let mut map = Mapping::new();
        let runs_on = self.runs_on();
        if !runs_on.is_empty() {
            let runs_on = if runs_on.len() == 1 {
                Value::String(runs_on[0].as_str().to_string())
            } else {
                Value::Sequence(
                    runs_on
                        .iter()
                        .map(|r| Value::String(r.as_str().to_string()))
                        .collect(),
                )
            };
            map.insert(Value::String("runs-on".to_string()), runs_on);
        }

        if let Some(needs) = self.needs() {
            if needs.len() == 1 {
                map.insert(
                    Value::String("needs".to_string()),
                    Value::String(needs[0].to_string()),
                );
            } else {
                map.insert(
                    Value::String("needs".to_string()),
                    Value::Sequence(needs.iter().map(|n| Value::String(n.to_string())).collect()),
                );
            }
        }

        map.insert(
            Value::String("steps".to_string()),
            Value::Sequence(self.steps().iter().map(|s| s.to_yaml()).collect()),
        );
        Value::Mapping(map)
    }
}
