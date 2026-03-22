use super::define_step::Step;
use super::define_step::UseStep;

#[derive(Debug)]
pub struct SetupRustStep {
    add_wasm32_wasip1_threads_target: bool,
    add_cargo_codspeed_bin: bool,
}

impl UseStep for SetupRustStep {
    fn name(&self) -> &str {
        "Set up Rust"
    }
    fn uses_action_name(&self) -> &str {
        "moonrepo/setup-rust"
    }
    fn uses_action_version(&self) -> Option<&str> {
        Some("v1.3")
    }
    fn with(&self) -> Option<Vec<(String, serde_yaml::Value)>> {
        let mut with = vec![];
        if self.add_wasm32_wasip1_threads_target {
            with.push((
                "targets".to_string(),
                serde_yaml::Value::String("wasm32-wasip1-threads".to_string()),
            ));
            // with.push((
            //     "profile".to_string(),
            //     serde_yaml::Value::String("complete".to_string()),
            // ));
        }
        if self.add_cargo_codspeed_bin {
            with.push((
                "bins".to_string(),
                serde_yaml::Value::String("cargo-codspeed".to_string()),
            ));
        }
        with.push((
            "inherit-toolchain".to_string(),
            serde_yaml::Value::Bool(true),
        ));
        Some(with)
    }
}

pub fn setup_rust_step(
    add_wasm32_wasip1_threads_target: bool,
    add_cargo_codspeed_bin: bool,
) -> Step {
    Step::Uses(Box::new(SetupRustStep {
        add_wasm32_wasip1_threads_target,
        add_cargo_codspeed_bin,
    }))
}

#[test]
fn test_setup_rust_step_0() {
    use super::define_generate_yaml::GenerateGithubYaml;
    let step = setup_rust_step(true, false);
    let output = serde_yaml::to_string(&step.to_yaml()).unwrap();
    expect_test::expect![[r#"
        name: Set up Rust
        uses: moonrepo/setup-rust@v1.3
        with:
          targets: wasm32-wasip1-threads
          inherit-toolchain: true
    "#]]
    .assert_eq(&output);
}

#[test]
fn test_setup_rust_step_1() {
    use super::define_generate_yaml::GenerateGithubYaml;
    let step = setup_rust_step(false, false);
    let output = serde_yaml::to_string(&step.to_yaml()).unwrap();
    expect_test::expect![[r#"
        name: Set up Rust
        uses: moonrepo/setup-rust@v1.3
        with:
          inherit-toolchain: true
    "#]]
    .assert_eq(&output);
}

#[test]
fn test_setup_rust_step_2() {
    use super::define_generate_yaml::GenerateGithubYaml;
    let step = setup_rust_step(false, true);
    let output = serde_yaml::to_string(&step.to_yaml()).unwrap();
    expect_test::expect![[r#"
        name: Set up Rust
        uses: moonrepo/setup-rust@v1.3
        with:
          bins: cargo-codspeed
          inherit-toolchain: true
    "#]]
    .assert_eq(&output);
}
