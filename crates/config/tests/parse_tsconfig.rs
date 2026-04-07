use bolt_ts_config::parse_tsconfig;

#[test]
fn tsconfig_should_allow_comments() {
    let c = parse_tsconfig(
        r#"
{
  // This is a comment
  "compilerOptions": {
    "target": "es2022" // Another comment
  }
}"#,
    );
    assert!(c.is_ok());
}

#[test]
fn tsconfig_should_disallow_loose_object_property_name() {
    let c = parse_tsconfig(
        r#"
{
  // This is a comment
  "compilerOptions": {
    123: 456
  }
}"#,
    );
    assert!(c.is_err());

    let c = parse_tsconfig(
        r#"
{
  // This is a comment
  "compilerOptions": {
    abc: 456
  }
}"#,
    );
    assert!(c.is_err());
}

#[test]
fn tsconfig_should_allow_trailing_commas() {
    let c = parse_tsconfig(
        r#"
{
  // This is a comment
  "compilerOptions": {
    "abc": "def",
  }
}"#,
    );
    assert!(c.is_ok());
}

#[test]
fn tsconfig_should_disallow_missing_commas() {
    let c = parse_tsconfig(
        r#"
{
  // This is a comment
  "compilerOptions": {
    "abc": "def"
    "xyz": "uvw"
  }
}"#,
    );
    assert!(c.is_err());
}

#[test]
fn tsconfig_should_disallow_single_quoted_strings() {
    let c = parse_tsconfig(
        r#"
    {
      // This is a comment
      'compilerOptions': {
        "abc": 'def',
      }
    }"#,
    );
    assert!(c.is_err());

    let c = parse_tsconfig(
        r#"
    {
      // This is a comment
      'compilerOptions': {
        'abc': "def",
      }
    }"#,
    );
    assert!(c.is_err());
}

#[test]
fn tsconfig_should_disallow_hexadecimal_numbers() {
    let c = parse_tsconfig(
        r#"
    {
      // This is a comment
      'compilerOptions': {
        "abc": 0x2A,
      }
    }"#,
    );
    assert!(c.is_err());
}

#[test]
fn tsconfig_should_disallow_unary_plus_number() {
    let c = parse_tsconfig(
        r#"
    {
      // This is a comment
      'compilerOptions': {
        "abc": +42,
      }
    }"#,
    );
    assert!(c.is_err());
}
