use project_root::get_project_root;

const BENCH_REPO: &str = "https://github.com/bvanjoi/typescript-compiler-bench";
const BENCH_CASE_DIR_NAME: &str = "benchmarks";

fn clone_bench_repo() -> std::path::PathBuf {
    let repo_path = std::env::temp_dir().join("typescript-compiler-bench");
    git2::Repository::clone(BENCH_REPO, &repo_path).unwrap();
    println!("temp bench dir: {:?}", &repo_path);
    assert!(repo_path.join(BENCH_CASE_DIR_NAME).is_dir());
    repo_path
}

fn install_deps(cwd: &std::path::Path) {
    std::process::Command::new("pnpm")
        .arg("install")
        .current_dir(cwd)
        .status()
        .expect("Failed to run pnpm install");
}

#[derive(Clone)]
struct Case {
    name: String,
    dir: std::path::PathBuf,
}

impl std::fmt::Debug for Case {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Case {
    fn new(name: String, dir: std::path::PathBuf) -> Self {
        assert!(dir.is_dir());
        assert!(dir.is_absolute());
        Self { name, dir }
    }
}

fn list_bench_case(root: &std::path::Path) -> Vec<Case> {
    let benchmarks = root.join(BENCH_CASE_DIR_NAME);
    let dir = std::fs::read_dir(benchmarks).unwrap();
    dir.into_iter()
        .map(|entry| {
            let entry = entry.unwrap();
            let file_name = entry.file_name().to_string_lossy().to_string();
            Case::new(file_name, entry.path())
        })
        .collect()
}

fn setup() -> Vec<Case> {
    let p = clone_bench_repo();
    install_deps(&p);
    list_bench_case(&p)
}

static CASES: std::sync::LazyLock<Vec<Case>> = std::sync::LazyLock::new(setup);

fn compile(input_dir: std::path::PathBuf) {
    debug_assert!(input_dir.is_dir(), "'{input_dir:#?}' not found.",);
    let tsconfig_path = input_dir.join(bolt_ts_compiler::DEFAULT_TSCONFIG);
    debug_assert!(tsconfig_path.is_file());
    let tsconfig = {
        let s = std::fs::read_to_string(tsconfig_path).unwrap();
        let raw: bolt_ts_config::RawTsConfig = serde_json::from_str(&s).unwrap();
        raw.normalize()
    };
    // ==== atom init ====
    let mut atoms = bolt_ts_compiler::init_atom();
    // ==== fs init ====
    let fs = bolt_ts_fs::LocalFS::new(&mut atoms);
    let default_lib_dir = {
        let root = get_project_root().unwrap();
        root.join("crates")
            .join("libs")
            .join("src")
            .join("declared_file")
    };
    let default_libs = bolt_ts_libs::DEFAULT_LIBS
        .iter()
        .map(|filename| default_lib_dir.join(filename))
        .collect::<Vec<_>>();
    let output = bolt_ts_compiler::eval_with_fs(
        input_dir,
        &tsconfig,
        default_lib_dir,
        default_libs,
        fs,
        atoms,
    );
    assert!(output.diags.is_empty());
}

#[divan::bench(args = CASES.clone().into_iter())]
fn bench_compile(bencher: divan::Bencher, case: &Case) {
    bencher.bench(|| {
        compile(case.dir.clone());
    });
}

fn main() {
    divan::main();
}
