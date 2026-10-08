mod common;
pub mod errors;
mod header;
mod header_compiler_options;
mod parse_error_directive;
pub mod run_tests;
mod temp_dir;
mod temp_files;
mod test_props_builder;

pub use header_compiler_options::CompilerOptions;
pub use temp_dir::{TempDir, tempdir};
pub use temp_files::build_temp_files;
pub use test_props_builder::TestPropsKey;

pub fn ensure_node_exist() {
    let output = std::process::Command::new("node")
        .arg("-v")
        .output()
        .expect("Failed to execute command");
    if output.status.success() {
        let stdout = std::str::from_utf8(&output.stdout).expect("Failed to parse output");
        println!("Node version: {stdout}");
    } else {
        let stderr = std::str::from_utf8(&output.stderr).expect("Failed to parse output");
        panic!("Error:\n{stderr}");
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
    let temp_dir = tempdir("bolt-ts-compiler-temp");
    temp_dir.path().join(format!("{name}.js"))
}
