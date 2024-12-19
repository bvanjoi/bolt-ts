mod common;
pub mod errors;
mod fixtures;
mod header;
pub mod run_tests;

pub use common::TestConfig;
pub use fixtures::fixtures;
pub use header::TestProps;

pub fn ensure_node_exist() {
    let output = std::process::Command::new("node")
        .arg("-v")
        .output()
        .expect("Failed to execute command");
    if output.status.success() {
        let stdout = std::str::from_utf8(&output.stdout).expect("Failed to parse output");
        println!("Node version: {}", stdout);
    } else {
        let stderr = std::str::from_utf8(&output.stderr).expect("Failed to parse output");
        panic!("Error:\n{}", stderr);
    }
}

pub fn run_node(p: &std::path::Path) -> Result<Option<String>, String> {
    let run_output = std::process::Command::new("node")
        .arg(p)
        .output()
        .expect("Failed to execute node");

    if run_output.status.success() {
        let stdout = std::str::from_utf8(&run_output.stdout).expect("Failed to parse output");
        Ok((!stdout.is_empty()).then(|| stdout.to_string()))
    } else {
        let stderr = std::str::from_utf8(&run_output.stderr).expect("Failed to parse output");
        Err(stderr.to_string())
    }
}

pub fn temp_node_file(name: &str) -> std::path::PathBuf {
    let temp_dir = tempdir::TempDir::new("bolt-ts-compiler-temp").unwrap();
    temp_dir.into_path().join(format!("{}.js", name))
}
