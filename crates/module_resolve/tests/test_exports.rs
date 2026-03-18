mod utils;

use compile_test::build_temp_files;

use self::utils::Project;

fn deprecated_module_resolution_node10_with_exports_field() -> Project {
    let dir = build_temp_files(serde_json::json!({
      "./index.ts": "",
      "./tsconfig.json": r#"
{
  "compilerOptions": {
    "target": "es2015",
    "module": "commonjs",
		"moduleResolution": "node10"
  }
}"#,
      // dual
      "./node_modules/dual/package.json": r#"
{
  "name": "dual",
  "version": "1.0.0",
  "type": "module",
  "exports": "./dist/not-exist.js",
	"types": "./dist/index.d.ts"
}"#,
      "./node_modules/dual/dist/index.js": "",
      "./node_modules/dual/dist/index.d.ts": "",
    }));

    let tsconfig = std::fs::read_to_string(dir.path().join("tsconfig.json")).unwrap();
    let tsconfig: bolt_ts_config::RawTsConfig = serde_json::from_str(&tsconfig).unwrap();
    let tsconfig = tsconfig.normalize();

    Project::new(tsconfig, dir)
}

#[test]
fn test_exports_0() {
    const TARGET: &str = "dual";
    let root = deprecated_module_resolution_node10_with_exports_field();
    let from = root.dir_path().join("./index.ts");
    let expected = root.dir_path().join("./node_modules/dual/dist/index.d.ts");
    let expected = std::fs::canonicalize(expected).unwrap();
    root.should_eq(&from, TARGET, expected);
}
