use super::define_step::RunStep;
use super::define_step::Step;

#[derive(Debug)]
pub struct RunWasmPackBuildAndTestStep;

impl RunStep for RunWasmPackBuildAndTestStep {
    fn name(&self) -> &str {
        "Run wasm-pack build and test"
    }
    fn working_directory(&self) -> Option<&str> {
        Some("crates/wasm")
    }
    fn commands(&self) -> Vec<String> {
        vec![
            "cargo install wasm-pack".to_string(),
            "wasm-pack test --node".to_string(),
        ]
    }
}

pub fn run_wasm_pack_build_and_test_step() -> Step {
    Step::Run(Box::new(RunWasmPackBuildAndTestStep))
}

#[test]
fn test_run_wasm_pack_build_and_test_step() {
    use super::define_generate_yaml::GenerateGithubYaml;
    let step = run_wasm_pack_build_and_test_step();
    let output = serde_yaml::to_string(&step.to_yaml()).unwrap();
    expect_test::expect![[r#"
        name: Run wasm-pack build and test
        working-directory: crates/wasm
        run: |-
          cargo install wasm-pack
          wasm-pack test --node
    "#]]
    .assert_eq(&output);
}
