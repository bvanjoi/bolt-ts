mod utils;

use utils::should_eq;

#[test]
fn test_dir() {
    for t in ["./a", "./a/", "./a/index", "./a/index.ts"] {
        should_eq(
            serde_json::json!(
              {
                "/a/index.ts": "",
              }
            ),
            "/",
            t,
            "/a/index.ts",
        );
    }

    should_eq(
        serde_json::json!(
          {
            "/a/index.ts": "",
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
            "/a/": "",
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
            "/a/index.ts": "",
            "/a.ts": "",
          }
        ),
        "/",
        "./a.ts",
        "/a.ts",
    );

    for target in ["..", "../", "../index", "../index.ts"] {
        for base_dir in ["/a", "/a/"] {
            should_eq(
                serde_json::json!(
                  {
                    "/a/index.ts": "",
                    "/index.ts": "",
                  }
                ),
                base_dir,
                target,
                "/index.ts",
            );
        }
    }
}
