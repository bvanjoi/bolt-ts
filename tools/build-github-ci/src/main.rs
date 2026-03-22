mod define_generate_yaml;
mod define_job;
mod define_step;
mod define_task;

mod use_checkout_repo_step;
mod use_ci_task;
mod use_run_cargo_test_step;
mod use_run_codspeed;
mod use_run_pnpm_command;
mod use_run_wasm_pack_step;
mod use_setup_node_step;
mod use_setup_pnpm_step;
mod use_setup_rust_step;

fn github_workflow_yml_path(name: &str) -> std::path::PathBuf {
    let root = project_root::get_project_root().unwrap();
    root.join(".github/workflows").join(name)
}

fn ci_task_yml_path() -> std::path::PathBuf {
    github_workflow_yml_path("ci.yml")
}

fn main() {
    let ci_task = use_ci_task::CITask;
    let path = ci_task_yml_path();
    define_task::write_task_to_file(&ci_task, &path);
}

#[test]
fn assert_github_yaml_latest() {
    use self::define_task::task_to_yaml_value;

    let path = ci_task_yml_path();
    let actual = std::fs::read_to_string(&path).unwrap();
    let ci_task = use_ci_task::CITask;
    let expect = serde_yaml::to_string(&task_to_yaml_value(&ci_task)).unwrap();
    assert_eq!(actual, expect);
}
