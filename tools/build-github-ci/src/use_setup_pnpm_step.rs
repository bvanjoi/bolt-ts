use super::define_step::Step;
use super::define_step::UseStep;

#[derive(Debug)]
pub struct SetupPnpmStep {
    run_install: bool,
}

impl UseStep for SetupPnpmStep {
    fn name(&self) -> &str {
        "Set up pnpm"
    }
    fn uses_action_name(&self) -> &str {
        "pnpm/action-setup"
    }
    fn uses_action_version(&self) -> Option<&str> {
        Some("v4")
    }
    fn with(&self) -> Option<Vec<(String, serde_yaml::Value)>> {
        if self.run_install {
            Some(vec![(
                "run_install".to_string(),
                serde_yaml::Value::Bool(true),
            )])
        } else {
            None
        }
    }
}

pub fn setup_pnpm_step(run_install: bool) -> Step {
    Step::Uses(Box::new(SetupPnpmStep { run_install }))
}

#[test]
fn test_setup_pnpm_step() {
    use super::define_generate_yaml::GenerateGithubYaml;
    let step = setup_pnpm_step(false);
    let output = serde_yaml::to_string(&step.to_yaml()).unwrap();
    expect_test::expect![[r#"
        name: Set up pnpm
        uses: pnpm/action-setup@v4
    "#]]
    .assert_eq(&output);
}

#[test]
fn test_setup_pnpm_step_with_run_install() {
    use super::define_generate_yaml::GenerateGithubYaml;
    let step = setup_pnpm_step(true);
    let output = serde_yaml::to_string(&step.to_yaml()).unwrap();
    expect_test::expect![[r#"
        name: Set up pnpm
        uses: pnpm/action-setup@v4
        with:
          run_install: true
    "#]]
    .assert_eq(&output);
}
