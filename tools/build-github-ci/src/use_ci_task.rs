use super::define_job::Job;
use super::define_job::RunOn;
use super::define_step::Step;
use super::define_task::On;
use super::define_task::OnBuilder;
use super::define_task::Task;
use super::use_checkout_repo_step::checkout_repository_step;
use super::use_run_cargo_test_step::run_cargo_test_step;
use super::use_run_codspeed::codspeed_benchmark_steps;
use super::use_run_pnpm_command::run_biome_format_and_lint_command_step;
use super::use_run_pnpm_command::run_pnpm_build_command_step;
use super::use_run_pnpm_command::run_pnpm_test_command_step;
use super::use_run_wasm_pack_step::run_wasm_pack_build_and_test_step;
use super::use_setup_node_step::setup_node_step;
use super::use_setup_pnpm_step::setup_pnpm_step;
use super::use_setup_rust_step::setup_rust_step;

#[derive(Debug)]
pub struct CargoTestJob;

impl Job for CargoTestJob {
    fn job_id(&self) -> &str {
        "cargo-test"
    }
    fn runs_on(&self) -> Vec<RunOn> {
        vec![RunOn::UbuntuLatest]
    }
    fn steps(&self) -> Vec<Step> {
        vec![
            checkout_repository_step(),
            setup_rust_step(false, false),
            setup_node_step("24"),
            run_cargo_test_step(),
            run_wasm_pack_build_and_test_step(),
        ]
    }
}

pub struct JavaScriptCheckAndTestJob;

impl Job for JavaScriptCheckAndTestJob {
    fn job_id(&self) -> &str {
        "javascript-check-and-test"
    }
    fn runs_on(&self) -> Vec<RunOn> {
        vec![RunOn::UbuntuLatest]
    }
    fn steps(&self) -> Vec<Step> {
        vec![
            checkout_repository_step(),
            setup_rust_step(true, false),
            setup_node_step("24"),
            setup_pnpm_step(true),
            run_pnpm_build_command_step(),
            run_biome_format_and_lint_command_step(),
            run_pnpm_test_command_step(),
        ]
    }
}

pub struct CodSpeedBenchmarkJob;

impl Job for CodSpeedBenchmarkJob {
    fn job_id(&self) -> &str {
        "codspeed-benchmark"
    }
    fn runs_on(&self) -> Vec<RunOn> {
        vec![RunOn::UbuntuLatest]
    }
    fn steps(&self) -> Vec<Step> {
        let mut steps = vec![
            checkout_repository_step(),
            setup_rust_step(false, true),
            setup_node_step("24"),
            setup_pnpm_step(false),
        ];
        for step in codspeed_benchmark_steps() {
            steps.push(step);
        }
        steps
    }
}

#[derive(Debug)]
pub struct CITask;

impl Task for CITask {
    fn name(&self) -> &str {
        "CI"
    }
    fn on(&self) -> On {
        OnBuilder::new().with_push(true).build()
    }
    fn jobs(&self) -> Vec<Box<dyn Job>> {
        vec![
            Box::new(CargoTestJob),
            Box::new(JavaScriptCheckAndTestJob),
            Box::new(CodSpeedBenchmarkJob),
        ]
    }
}
