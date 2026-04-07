use super::define_step::RunStep;
use super::define_step::Step;
use super::define_step::UseStep;

#[derive(Debug)]
pub struct CodSpeedBenchmarkStep;

impl UseStep for CodSpeedBenchmarkStep {
    fn name(&self) -> &str {
        "Run CodSpeed benchmark"
    }

    fn uses_action_name(&self) -> &str {
        "CodSpeedHQ/action"
    }

    fn uses_action_version(&self) -> Option<&str> {
        Some("v4")
    }

    fn with(&self) -> Option<Vec<(String, serde_yaml::Value)>> {
        Some(vec![
            (
                "run".to_string(),
                // `bench` is the name `crates/compiler/benches/bench`
                serde_yaml::Value::String("cargo codspeed run bench -m walltime".to_string()),
            ),
            (
                "token".to_string(),
                serde_yaml::Value::String("${{ secrets.CODSPEED_TOKEN }}".to_string()),
            ),
            (
                "mode".to_string(),
                serde_yaml::Value::String("walltime".to_string()),
            ),
            (
                "cache-instruments".to_string(),
                serde_yaml::Value::String("false".to_string()),
            ),
        ])
    }
}

#[derive(Debug)]
pub struct RunCodSpeedBuildStep;

impl RunStep for RunCodSpeedBuildStep {
    fn name(&self) -> &str {
        "Run CodSpeed build"
    }

    fn commands(&self) -> Vec<String> {
        vec!["cargo codspeed build -m walltime".to_string()]
    }
}

pub fn codspeed_benchmark_steps() -> Vec<Step> {
    vec![
        Step::Run(Box::new(RunCodSpeedBuildStep)),
        Step::Uses(Box::new(CodSpeedBenchmarkStep)),
    ]
}
