mod utils;

use utils::should_eq;

#[test]
fn test_symlinks() {
    should_eq(
        serde_json::json!({
          "/node_modules/a/a.ts": "",
          "/node_modules/a/b.ts": "symlink:/node_modules/a/a.ts",
        }),
        "/",
        "a/b",
        "/node_modules/a/a.ts",
    );
}
