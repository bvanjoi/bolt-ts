mod utils;

use compile_test::build_temp_files;

use self::utils::should_eq;
use self::utils::should_not_found;

#[test]
fn test_dir_for_ignore_index() {
    let dir = build_temp_files(serde_json::json!(
      {
        "./a/index.ts": "",
        "./index.ts": "",
      }
    ));
    let dir = dir.path();
    let from = dir.join("./index.ts");
    let expected = dir.join("./a/index.ts");
    should_eq(&from, "./a", expected.clone());
    should_eq(&from, "./a/", expected.clone());
    should_eq(&from, "./a/index", expected.clone());
    should_eq(&from, "./a/index.ts", expected.clone());
    should_not_found(&from, "./a/i");
}

#[test]
fn test_dir_but_has_same_name_file_1() {
    let dir = build_temp_files(serde_json::json!(
      {
        "./a/index.ts": "",
        "./a.ts": "",
        "./index.ts": "",
      }
    ));
    let dir = dir.path();
    let from = dir.join("./index.ts");

    should_eq(&from, "./a", dir.join("./a.ts"));
    should_eq(&from, "./a.ts", dir.join("./a.ts"));
    should_eq(&from, "./a.js", dir.join("./a.ts"));
    should_eq(&from, "./a/", dir.join("./a/index.ts"));
}

#[test]
fn test_dir_but_has_same_name_file_2() {
    let dir = build_temp_files(serde_json::json!(
      {
        "./a/": "",
        "./a.ts": "",
        "./index.ts": "",
      }
    ));
    let dir = dir.path();
    let from = dir.join("./index.ts");
    should_eq(&from, "./a", dir.join("./a.ts"));
    should_not_found(&from, "./a/");
}

#[test]
fn test_dir_from_child_dir() {
    let dir = build_temp_files(serde_json::json!(
      {
        "./a/index.ts": "",
        "./b/index.ts": "",
        "./index.ts": "",
      }
    ));
    let dir = dir.path();
    let from = dir.join("./b/index.ts");
    should_eq(&from, "../index", dir.join("./index.ts"));
    should_eq(&from, "../index.ts", dir.join("./index.ts"));
    should_eq(&from, "../b", dir.join("./b/index.ts"));
    should_eq(&from, "../b/", dir.join("./b/index.ts"));
    should_eq(&from, "../b/index", dir.join("./b/index.ts"));
    should_eq(&from, "../b/index.ts", dir.join("./b/index.ts"));
    should_eq(&from, "../a", dir.join("./a/index.ts"));
    should_eq(&from, "../a/", dir.join("./a/index.ts"));
    should_eq(&from, "../a/index", dir.join("./a/index.ts"));
    should_eq(&from, "../a/index.ts", dir.join("./a/index.ts"));
}
