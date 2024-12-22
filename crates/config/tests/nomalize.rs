use bolt_ts_config::{NormalizedTsConfig, TsConfig};

fn normalize(input: &str) -> NormalizedTsConfig {
    let raw: TsConfig = serde_json::from_str(input).unwrap();
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
