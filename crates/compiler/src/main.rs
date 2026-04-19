use bolt_ts_compiler::{current_exe_dir, eval_with_fs, init_atom};
use bolt_ts_config::{RawTsConfig, parse_tsconfig};
use bolt_ts_fs::CachedFileSystem;
use std::env;
use std::path::{Path, PathBuf};

fn get_absolute_path(input_path: &str) -> PathBuf {
    let path = Path::new(input_path);

    if path.is_absolute() {
        path.to_path_buf()
    } else {
        let cwd = env::current_dir().unwrap();
        cwd.join(path)
    }
}

fn main() {
    let start = std::time::Instant::now();
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <path>", args[0]);
        std::process::exit(1);
    }

    let input_path = &args[1];
    let p = get_absolute_path(input_path);
    let mut atoms = init_atom();
    let mut fs = bolt_ts_fs::LocalFS::new(&mut atoms);
    let exe_dir = current_exe_dir();
    let libs = bolt_ts_libs::DEFAULT_LIBS
        .iter()
        .map(|filename| exe_dir.join(filename))
        .collect::<Vec<_>>();
    let tsconfig = if p.ends_with("tsconfig.json") {
        let content = fs.read_file(&p, &mut atoms).unwrap();
        let s = atoms.get(content);
        parse_tsconfig(s).unwrap()
    } else {
        RawTsConfig::default().with_include(vec![p.to_str().unwrap().to_string()])
    };
    let cwd = env::current_dir().unwrap();
    let tsconfig = tsconfig.normalize();
    let parser_herd = bolt_ts_arena::bumpalo_herd::Herd::new();
    let type_arena = bolt_ts_arena::bumpalo::Bump::new();
    let mut compiler_result = eval_with_fs(
        cwd,
        tsconfig,
        exe_dir,
        libs,
        &parser_herd,
        &type_arena,
        fs,
        atoms,
    );
    let module_arena = compiler_result.steal_module_arena();
    let diags = compiler_result.steal_diags();
    diags.into_iter().for_each(|diag| diag.emit(&module_arena));
    let duration = start.elapsed();
    println!("Files: {}", module_arena.modules().len());
    println!("Types: {}", compiler_result.type_count());
    println!(
        "Time cost: {}",
        pretty_duration::pretty_duration(&duration, None)
    );
}
