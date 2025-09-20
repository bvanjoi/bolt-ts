use bolt_ts_compiler::{current_exe_dir, eval_with_fs, init_atom};
use bolt_ts_config::RawTsConfig;
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
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <path>", args[0]);
        std::process::exit(1);
    }

    let input_path = &args[1];
    let start = std::time::Instant::now();
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
        serde_json::from_str(s).unwrap()
    } else {
        RawTsConfig::default().with_include(vec![p.to_str().unwrap().to_string()])
    };
    let cwd = env::current_dir().unwrap();
    let tsconfig = tsconfig.normalize();
    let output = eval_with_fs(cwd, &tsconfig, exe_dir, libs, fs, atoms);
    let duration = start.elapsed();
    output
        .diags
        .into_iter()
        .for_each(|diag| diag.emit(&output.module_arena));
    println!("Files: {}", output.module_arena.modules().len());
    println!("Types: {}", output.types_len);
    println!(
        "Time cost: {}",
        pretty_duration::pretty_duration(&duration, None)
    );
}
