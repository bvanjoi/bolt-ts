use bolt_ts_config::{NormalizedTsConfig, RawTsConfig};

fn normalize(input: &str) -> NormalizedTsConfig {
    let raw: RawTsConfig = serde_json::from_str(input).unwrap();
    raw.normalize()
}

#[test]
fn test_default_compiler_options() {
    let c = normalize("{}");
    let co = c.compiler_options();
    assert!(!co.strict());
    assert!(!co.always_strict());
    assert!(!co.preserve_symlinks());
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
fn test_strict_options_should_effect_0() {
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
}

#[test]
fn test_strict_options_should_effect_1() {
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

#[test]
fn test_use_define_for_class_fields() {
    let c = normalize(
        r#"
{
  "compilerOptions": {
    "target": "es2022",
    "useDefineForClassFields": true
  }
}"#,
    );
    assert!(c.compiler_options().use_define_for_class_fields());

    let c = normalize(
        r#"
{
  "compilerOptions": {
    "target": "es2022"
  }
}"#,
    );
    assert!(c.compiler_options().use_define_for_class_fields());

    let c = normalize(
        r#"
{
  "compilerOptions": {
    "target": "es2022",
    "useDefineForClassFields": false
  }
}"#,
    );
    assert!(!c.compiler_options().use_define_for_class_fields());
}
