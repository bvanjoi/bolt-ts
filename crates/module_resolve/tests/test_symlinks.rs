mod utils;

use compile_test::build_temp_files;
use utils::should_eq;

#[test]
fn test_symlinks() {
    let dir = build_temp_files(serde_json::json!({
      "./node_modules/a/a.ts": "",
      "./node_modules/a/b.ts": "symlink:./a.ts",
      "./index.ts": ""
    }));
    let dir = dir.path();
    let f = dir.join("./index.ts");
    should_eq(&f, "./node_modules/a/b", dir.join("./node_modules/a/b.ts"));
    let expected = std::fs::canonicalize(dir.join("./node_modules/a/a.ts")).unwrap();
    should_eq(&f, "a/b", expected);
}
