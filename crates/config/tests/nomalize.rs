use bolt_ts_config::{NormalizedTsConfig, RawTsConfig};

fn normalize(input: &str) -> NormalizedTsConfig {
    let raw: RawTsConfig = serde_json::from_str(input).unwrap();
    raw.normalize()
}

#[test]
fn test_default_include() {
    let c = normalize("{}");
    assert_eq!(c.include(), &["**/*"]);

    let c = normalize(
        r#"
{
  "include": []
}
    "#,
    );
    assert!(c.include().is_empty());
}

#[test]
fn strict_should_effect() {
    let c = normalize(
        r#"
{
  "compilerOptions": {
    "strict": true
  }
}"#,
    );
    assert!(c.compiler_options().strict());
    assert!(c.compiler_options().always_strict());

    let c = normalize(
        r#"
{
  "compilerOptions": {
    "strict": true,
    "alwaysStrict": false 
  }
}"#,
    );
    assert!(c.compiler_options().strict());
    assert!(!c.compiler_options().always_strict());
}

#[test]
fn test_target_fields() {
    let c = normalize(
        r#"
{
  "compilerOptions": {
    "target": "es2022"
  }    
}"#,
    );
    assert_eq!(
        *c.compiler_options().target(),
        bolt_ts_config::Target::ES2022
    );

    let c = normalize(
        r#"
{
  "compilerOptions": {
    "target": "ES2022"
  }    
}"#,
    );
    assert_eq!(
        *c.compiler_options().target(),
        bolt_ts_config::Target::ES2022
    );
}
