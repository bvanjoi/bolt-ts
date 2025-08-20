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

    for target in ["./b", "./b.d"] {
        should_eq(
            serde_json::json!(
              {
                "/b.d.ts": "",
              }
            ),
            "/",
            target,
            "/b.d.ts",
        );
    }

    should_eq(
        serde_json::json!(
          {
            "/b.ts": "",
            "/b.d.ts": "",
          }
        ),
        "/",
        "./b",
        "/b.ts",
    );
}
