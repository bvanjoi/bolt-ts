use bolt_ts_config::parse_tsconfig;
use bolt_ts_fs::CachedFileSystem;
use project_root::get_project_root;

const BENCH_REPO: &str = "https://github.com/bvanjoi/typescript-compiler-bench";
const PROJECT_BENCH_CASE_DIR_NAME: &str = "project-benchmarks";
const SINGLE_BENCH_CASE_DIR_NAME: &str = "materials";

fn clone_bench_repo() -> std::path::PathBuf {
    let repo_path = std::env::temp_dir().join("typescript-compiler-bench");
    std::process::Command::new("git")
        .arg("clone")
        .arg(BENCH_REPO)
        .arg(&repo_path)
        .status()
        .expect("Failed to clone bench repo");
    println!("temp bench dir: {:?}", &repo_path);
    assert!(repo_path.join(PROJECT_BENCH_CASE_DIR_NAME).is_dir());
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
enum CaseKind {
    Project { dir: std::path::PathBuf },
    Single { file: std::path::PathBuf },
}
#[derive(Clone)]
struct Case {
    name: String,
    kind: CaseKind,
}

impl std::fmt::Debug for Case {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

fn list_bench_case(root: &std::path::Path) -> Vec<Case> {
    let project_benchmarks_dir = root.join(PROJECT_BENCH_CASE_DIR_NAME);
    let dir = std::fs::read_dir(project_benchmarks_dir).unwrap();
    let project_benchmarks = dir
        .into_iter()
        .map(|entry| {
            let entry = entry.unwrap();
            let file_name = entry.file_name().to_string_lossy().to_string();
            let dir = entry.path();
            assert!(dir.is_dir());
            assert!(dir.is_absolute());
            Case {
                name: file_name,
                kind: CaseKind::Project { dir },
            }
        })
        .collect::<Vec<_>>();

    let single_benchmark_dir = root.join(SINGLE_BENCH_CASE_DIR_NAME);
    let dir = std::fs::read_dir(single_benchmark_dir).unwrap();
    let single_benchmarks = dir.into_iter().map(|entry| {
        let entry = entry.unwrap();
        let file_name = entry.file_name().to_string_lossy().to_string();
        let file = entry.path();
        assert!(file.is_file());
        assert!(file.is_absolute());
        Case {
            name: file_name,
            kind: CaseKind::Single { file },
        }
    });

    project_benchmarks
        .into_iter()
        .chain(single_benchmarks.into_iter())
        .collect()
}

fn setup() -> Vec<Case> {
    let p = clone_bench_repo();
    install_deps(&p);
    list_bench_case(&p)
}

static CASES: std::sync::LazyLock<Vec<Case>> = std::sync::LazyLock::new(setup);

fn compile(input_dir: std::path::PathBuf) {
    assert!(input_dir.is_dir(), "'{input_dir:#?}' not found.",);
    let tsconfig_path = input_dir.join(bolt_ts_compiler::DEFAULT_TSCONFIG);
    assert!(tsconfig_path.is_file());
    let tsconfig = {
        let s = std::fs::read_to_string(tsconfig_path).unwrap();
        let raw: bolt_ts_config::RawTsConfig = parse_tsconfig(&s).unwrap();
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
    let parser_arena = bolt_ts_arena::bumpalo_herd::Herd::new();
    let type_arena = bolt_ts_arena::bumpalo::Bump::new();
    let mut output = bolt_ts_compiler::eval_with_fs(
        input_dir,
        tsconfig,
        default_lib_dir,
        default_libs,
        &parser_arena,
        &type_arena,
        fs,
        atoms,
    );
    assert!(output.steal_diags().is_empty());
}

fn parse(input: std::path::PathBuf) {
    let parser_arena = bolt_ts_arena::bumpalo_herd::Herd::new();
    let mut atoms = bolt_ts_ast::keyword::init_atom_map();
    let mut module_arena = bolt_ts_span::ModuleArena::new(4);
    let mut fs = bolt_ts_fs::LocalFS::new(&mut atoms);
    let module_id = module_arena.new_module(
        input,
        false,
        |p, atoms| fs.read_file(p, atoms).ok(),
        &mut atoms,
    );
    let atoms = std::sync::Arc::new(std::sync::Mutex::new(atoms));
    let input = module_arena.get_content(module_id);
    let result = bolt_ts_parser::parse(
        atoms,
        &parser_arena.get(),
        input.as_bytes(),
        module_id,
        &module_arena,
        true,
    );
    assert!(result.diags.is_empty());
}

#[divan::bench(args = CASES.clone().into_iter(), sample_size = 1, sample_count = 10)]
fn bench_project(bencher: divan::Bencher, case: &Case) {
    let CaseKind::Project { dir } = &case.kind else {
        return;
    };
    bencher.bench(|| {
        compile(dir.clone());
        divan::black_box(());
    });
}

#[divan::bench(args = CASES.clone().into_iter(), sample_size = 1, sample_count = 10)]
fn bench_parse(bencher: divan::Bencher, case: &Case) {
    let CaseKind::Single { file } = &case.kind else {
        return;
    };
    bencher.bench(|| {
        parse(file.clone());
        divan::black_box(());
    });
}

fn main() {
    divan::main();
}
