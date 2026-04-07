use super::define_step::RunStep;
use super::define_step::Step;

#[derive(Debug)]
pub struct RunCargoTestStep;

impl RunStep for RunCargoTestStep {
    fn name(&self) -> &str {
        "Run cargo test"
    }
    fn commands(&self) -> Vec<String> {
        vec!["cargo test -q".to_string()]
    }
}

pub fn run_cargo_test_step() -> Step {
    Step::Run(Box::new(RunCargoTestStep))
}

#[test]
fn test_run_cargo_test_step() {
    use super::define_generate_yaml::GenerateGithubYaml;
    let step = run_cargo_test_step();
    let output = serde_yaml::to_string(&step.to_yaml()).unwrap();
    expect_test::expect![[r#"
        name: Run cargo test
        run: cargo test -q
    "#]]
    .assert_eq(&output);
}
