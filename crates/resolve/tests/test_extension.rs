mod utils;

use utils::should_eq;

#[test]
fn test_resolve_with_extension() {
    let map = || {
        serde_json::json!(
          {
            "/a.ts": "",
          }
        )
    };
    should_eq(map(), "/", "a", "/a.ts");
}
