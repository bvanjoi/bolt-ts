use super::define_step::Step;
use super::define_step::UseStep;

#[derive(Debug)]
pub struct CheckoutRepositoryStep;

impl UseStep for CheckoutRepositoryStep {
    fn name(&self) -> &str {
        "Checkout repository"
    }
    fn uses_action_name(&self) -> &str {
        "actions/checkout"
    }
    fn uses_action_version(&self) -> Option<&str> {
        Some("v2")
    }

    fn with(&self) -> Option<Vec<(String, serde_yaml::Value)>> {
        None
    }
}

pub fn checkout_repository_step() -> Step {
    Step::Uses(Box::new(CheckoutRepositoryStep))
}

#[test]
fn test_checkout_repository_step() {
    use super::define_generate_yaml::GenerateGithubYaml;
    let step = checkout_repository_step();
    let output = serde_yaml::to_string(&step.to_yaml()).unwrap();
    expect_test::expect![[r#"
        name: Checkout repository
        uses: actions/checkout@v2
    "#]]
    .assert_eq(&output);
}
