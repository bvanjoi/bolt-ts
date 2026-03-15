mod utils;

use compile_test::build_temp_files;

use self::utils::should_eq;

#[test]
fn test_auto_ts_extension() {
    let dir = build_temp_files(serde_json::json!(
      {
        "./a.ts": "",
        "./folder/test.ts": "",
        "./index.ts": "",
      }
    ));

    let dir = dir.path();
    let from = dir.join("./index.ts");
    should_eq(&from, "./a", dir.join("./a.ts"));
    should_eq(&from, "./folder/test", dir.join("./folder/test.ts"));
}

#[test]
fn test_auto_dts_extension() {
    let dir = build_temp_files(serde_json::json!(
      {
        "./b.d.ts": "",
        "./index.ts": "",
      }
    ));

    let dir = dir.path();
    let from = dir.join("./index.ts");
    should_eq(&from, "./b", dir.join("./b.d.ts"));
    should_eq(&from, "./b.d", dir.join("./b.d.ts"));
    should_eq(&from, "./b.d.ts", dir.join("./b.d.ts"));
}

#[test]
fn test_ts_extension_priority() {
    let dir = build_temp_files(serde_json::json!(
      {
        "./b.d.ts": "",
        "./b.ts": "",
        "./index.ts": "",
      }
    ));

    let dir = dir.path();
    let from = dir.join("./index.ts");
    should_eq(&from, "./b", dir.join("./b.ts"));
    should_eq(&from, "./b.ts", dir.join("./b.ts"));
    should_eq(&from, "./b.d", dir.join("./b.d.ts"));
    should_eq(&from, "./b.d.ts", dir.join("./b.ts"));
}
