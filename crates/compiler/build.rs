const LIB_RESOURCE_RELATIVE_PATH: &str = "crates/libs/src/declared_file";

fn main() {
    let out_dir = std::env::var("OUT_DIR").expect("OUT_DIR not set");
    let out_dir = std::path::Path::new(&out_dir);
    let out_dir = out_dir
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    let root = project_root::get_project_root().unwrap();
    let lib_resource_dir = root.join(LIB_RESOURCE_RELATIVE_PATH);
    assert!(lib_resource_dir.is_dir());
    for entry in std::fs::read_dir(lib_resource_dir).unwrap() {
        let p = entry.unwrap().path();
        assert!(p.is_file());
        let filename = p.file_name().unwrap();
        {
            let out_dir = out_dir.join("deps");
            let target = out_dir.join(filename);
            if target.is_file() {
                let _ = std::fs::remove_file(target.as_path());
            }
            std::fs::copy(&p, target).unwrap();
        }
        {
            let target = out_dir.join(filename);
            if target.is_file() {
                let _ = std::fs::remove_file(target.as_path());
            }
            std::fs::copy(&p, target).unwrap();
        }
    }
}
