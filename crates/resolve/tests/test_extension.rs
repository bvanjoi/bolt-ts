mod utils;

use utils::should_eq;

#[test]
fn test_resolve_with_extension() {
    should_eq(
        serde_json::json!(
          {
            "/a.ts": "",
          }
        ),
        "/",
        "./a",
        "/a.ts",
    );

    should_eq(
        serde_json::json!(
          {
            "/folder/test.ts": "",
          }
        ),
        "/",
        "./folder/test",
        "/folder/test.ts",
    );
}
