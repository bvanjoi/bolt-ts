use bolt_ts_module_resolve_test::should_not_found;
use compile_test::build_temp_files;

#[test]
fn test_resolve_with_not_found() {
    let map = serde_json::json!({
        "./index.ts": "",
    });
    let dir = build_temp_files(map);
    let from = dir.path().join("./index.ts");
    should_not_found(&from, "a");
    should_not_found(&from, "./a");
    should_not_found(&from, "./a.ts");
    should_not_found(&from, "./a/index");
    should_not_found(&from, "./a/index.ts");
}
