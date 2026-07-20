use super::define_step::RunStep;
use super::define_step::Step;

#[derive(Debug)]
pub enum RunCargoStep {
    Test,
    FormatCheck,
    ClippyCheck,
}

impl RunStep for RunCargoStep {
    fn name(&self) -> &str {
        match self {
            RunCargoStep::Test => "Run cargo test",
            RunCargoStep::FormatCheck => "Run cargo fmt check",
            RunCargoStep::ClippyCheck => "Run cargo clippy check",
        }
    }

    fn commands(&self) -> Vec<String> {
        match self {
            RunCargoStep::Test => vec!["cargo test -q".to_string()],
            RunCargoStep::FormatCheck => vec!["cargo fmt -- --check".to_string()],
            RunCargoStep::ClippyCheck => {
                vec!["cargo clippy --all-targets --all-features -- --deny warnings".to_string()]
            }
        }
    }
}

pub fn run_cargo_command_steps() -> Vec<Step> {
    vec![
        Step::Run(Box::new(RunCargoStep::FormatCheck)),
        Step::Run(Box::new(RunCargoStep::ClippyCheck)),
        Step::Run(Box::new(RunCargoStep::Test)),
    ]
}
