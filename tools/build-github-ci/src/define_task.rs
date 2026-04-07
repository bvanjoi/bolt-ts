use super::define_generate_yaml::GenerateGithubYaml;
use super::define_job::Job;

use serde_yaml::{Mapping, Value};

pub struct OnBuilder {
    push: Option<bool>,
}

impl OnBuilder {
    pub fn new() -> Self {
        Self { push: None }
    }

    pub fn with_push(mut self, push: bool) -> Self {
        self.push = Some(push);
        self
    }

    pub fn build(self) -> On {
        On {
            push: self.push.unwrap_or(false),
        }
    }
}

pub struct On {
    push: bool,
}

pub trait Task {
    fn name(&self) -> &str;
    fn jobs(&self) -> Vec<Box<dyn Job>>;
    fn on(&self) -> On;
}

impl GenerateGithubYaml for &dyn Task {
    fn to_yaml(&self) -> Value {
        let mut map = Mapping::new();

        map.insert(
            Value::String("name".to_string()),
            Value::String(self.name().to_string()),
        );

        let on = self.on();
        if on.push {
            map.insert(
                Value::String("on".to_string()),
                Value::String("push".to_string()),
            );
        }

        let jobs = self.jobs();
        if !jobs.is_empty() {
            map.insert(
                Value::String("jobs".to_string()),
                Value::Mapping(Mapping::from_iter(jobs.iter().map(|j| {
                    (Value::String(j.job_id().to_string()), j.as_ref().to_yaml())
                }))),
            );
        }
        Value::Mapping(map)
    }
}

pub fn task_to_yaml_value(task: &dyn Task) -> Value {
    task.to_yaml()
}

pub fn write_task_to_file(task: &dyn Task, path: &std::path::Path) {
    let yaml = task_to_yaml_value(task);
    let mut file = std::fs::File::create(path).unwrap();
    serde_yaml::to_writer(&mut file, &yaml).unwrap();
}

#[test]
fn test_task_0() {
    struct TaskStruct;
    impl Task for TaskStruct {
        fn name(&self) -> &str {
            "Test"
        }
        fn jobs(&self) -> Vec<Box<dyn Job>> {
            vec![]
        }
        fn on(&self) -> On {
            OnBuilder::new().with_push(true).build()
        }
    }
    let task = TaskStruct;
    let output = serde_yaml::to_string(&task_to_yaml_value(&task)).unwrap();
    expect_test::expect![[r#"
        name: Test
        on: push
    "#]]
    .assert_eq(&output);
}

#[test]
fn test_task_1() {
    struct Job1Struct;
    impl Job for Job1Struct {
        fn job_id(&self) -> &str {
            "job_1"
        }
        fn runs_on(&self) -> Vec<super::define_job::RunOn> {
            vec![]
        }
        fn steps(&self) -> Vec<super::define_step::Step> {
            vec![]
        }
    }
    struct Job2Struct;
    impl Job for Job2Struct {
        fn job_id(&self) -> &str {
            "job_2"
        }
        fn runs_on(&self) -> Vec<super::define_job::RunOn> {
            vec![]
        }
        fn steps(&self) -> Vec<super::define_step::Step> {
            vec![]
        }
    }
    struct TaskStruct;
    impl Task for TaskStruct {
        fn name(&self) -> &str {
            "Test"
        }
        fn jobs(&self) -> Vec<Box<dyn Job>> {
            let job1 = Box::new(Job1Struct);
            let job2 = Box::new(Job2Struct);
            vec![job1, job2]
        }
        fn on(&self) -> On {
            OnBuilder::new().with_push(true).build()
        }
    }

    let task = TaskStruct;
    let output = serde_yaml::to_string(&task_to_yaml_value(&task)).unwrap();
    expect_test::expect![[r#"
        name: Test
        on: push
        jobs:
          job_1:
            steps: []
          job_2:
            steps: []
    "#]]
    .assert_eq(&output);
}
