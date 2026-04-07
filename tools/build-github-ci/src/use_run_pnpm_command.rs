use super::define_step::RunStep;
use super::define_step::Step;
use super::define_step::UseStep;

#[derive(Debug)]
pub struct RunPnpmBuildCommandStep;

impl RunStep for RunPnpmBuildCommandStep {
    fn name(&self) -> &str {
        "Run pnpm build packages"
    }
    fn commands(&self) -> Vec<String> {
        vec!["pnpm build".to_string()]
    }
}

pub fn run_pnpm_build_command_step() -> Step {
    Step::Run(Box::new(RunPnpmBuildCommandStep))
}

#[test]
fn test_run_pnpm_build_command_step() {
    use super::define_generate_yaml::GenerateGithubYaml;
    let step = run_pnpm_build_command_step();
    let output = serde_yaml::to_string(&step.to_yaml()).unwrap();
    expect_test::expect![[r#"
        name: Run pnpm build packages
        run: pnpm build
    "#]]
    .assert_eq(&output);
}

#[derive(Debug)]
pub struct RunPnpmTestCommandStep;

impl UseStep for RunPnpmTestCommandStep {
    fn name(&self) -> &str {
        "Test packages under headless"
    }

    fn uses_action_name(&self) -> &str {
        "coactions/setup-xvfb"
    }

    fn uses_action_version(&self) -> Option<&str> {
        Some("v1")
    }

    fn with(&self) -> Option<Vec<(String, serde_yaml::Value)>> {
        Some(vec![(
            "run".to_string(),
            serde_yaml::Value::String("pnpm test".to_string()),
        )])
    }
}

pub fn run_pnpm_test_command_step() -> Step {
    Step::Uses(Box::new(RunPnpmTestCommandStep))
}

#[test]
fn test_run_pnpm_test_command_step() {
    use super::define_generate_yaml::GenerateGithubYaml;
    let step = run_pnpm_test_command_step();
    let output = serde_yaml::to_string(&step.to_yaml()).unwrap();
    expect_test::expect![[r#"
        name: Test packages under headless
        uses: coactions/setup-xvfb@v1
        with:
          run: pnpm test
    "#]]
    .assert_eq(&output);
}

#[derive(Debug)]
pub struct BiomeFormatAndLintCommandStep;

impl RunStep for BiomeFormatAndLintCommandStep {
    fn name(&self) -> &str {
        "Run biome format and lint check"
    }

    fn commands(&self) -> Vec<String> {
        vec!["npx biome format".to_string(), "npx biome lint".to_string()]
    }
}

pub fn run_biome_format_and_lint_command_step() -> Step {
    Step::Run(Box::new(BiomeFormatAndLintCommandStep))
}
