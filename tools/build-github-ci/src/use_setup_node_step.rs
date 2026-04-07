use super::define_step::Step;
use super::define_step::UseStep;

#[derive(Debug)]
pub struct SetupNodeStep {
    node_version: String,
}

impl UseStep for SetupNodeStep {
    fn name(&self) -> &str {
        "Set up Node"
    }
    fn uses_action_name(&self) -> &str {
        "actions/setup-node"
    }
    fn uses_action_version(&self) -> Option<&str> {
        Some("v4")
    }
    fn with(&self) -> Option<Vec<(String, serde_yaml::Value)>> {
        let node_version = (
            "node-version".to_string(),
            serde_yaml::Value::String(self.node_version.clone()),
        );
        Some(vec![node_version])
    }
}

pub fn setup_node_step(node_version: &str) -> Step {
    Step::Uses(Box::new(SetupNodeStep {
        node_version: node_version.to_string(),
    }))
}

#[test]
fn test_setup_node_step() {
    use super::define_generate_yaml::GenerateGithubYaml;
    let step = setup_node_step("24");
    let output = serde_yaml::to_string(&step.to_yaml()).unwrap();
    expect_test::expect![[r#"
        name: Set up Node
        uses: actions/setup-node@v4
        with:
          node-version: '24'
    "#]]
    .assert_eq(&output);
}
