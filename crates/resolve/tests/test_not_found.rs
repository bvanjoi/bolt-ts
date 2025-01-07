mod utils;

use utils::should_not_found;

#[test]
fn test_resolve_with_not_found() {
    let map = || serde_json::json!({});
    should_not_found(map(), "/", "a");
    should_not_found(map(), "/", "./a");
    should_not_found(map(), "/", "./a.ts");
}
