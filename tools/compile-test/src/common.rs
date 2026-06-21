use std::fmt;
use std::str::FromStr;

macro_rules! string_enum {
    ($(#[$meta:meta])* $vis:vis enum $name:ident { $($variant:ident => $repr:expr,)* }) => {
        $(#[$meta])*
        $vis enum $name {
            $($variant,)*
        }

        impl $name {
            $vis const VARIANTS: &'static [Self] = &[$(Self::$variant,)*];
            $vis const STR_VARIANTS: &'static [&'static str] = &[$(Self::$variant.as_str(),)*];

            $vis const fn as_str(&self) -> &'static str {
                match self {
                    $(Self::$variant => $repr,)*
                }
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Display::fmt(self.as_str(), f)
            }
        }

        impl FromStr for $name {
            type Err = ();

            fn from_str(s: &str) -> Result<Self, ()> {
                match s {
                    $($repr => Ok(Self::$variant),)*
                    _ => Err(()),
                }
            }
        }
    }
}

string_enum! {
    #[derive(Clone, Copy, PartialEq, Debug, Hash)]
    pub enum PassMode {
        Check => "check",
        Build => "build",
        Run => "run",
    }
}

#[derive(Clone, Copy, PartialEq, Debug, Hash)]
pub enum FailMode {
    Check,
    Build,
    Run,
}

#[derive(Default)]
pub struct TestConfig {
    pub(super) compiler_options: CompilerOptions,
}

#[derive(Debug, Default)]
pub struct CompilerOptions(std::collections::HashMap<String, CompilerOption>);

impl CompilerOptions {
    pub(super) fn len(&self) -> usize {
        self.0.len()
    }

    pub(super) fn extend<T: IntoIterator<Item = (String, CompilerOption)>>(&mut self, iter: T) {
        self.0.extend(iter)
    }

    pub fn to_serde_json(
        &self,
    ) -> Vec<(serde_json::Map<String, serde_json::Value>, Option<String>)> {
        fn dfs(
            result: &mut Vec<(serde_json::Map<String, serde_json::Value>, Option<String>)>,
            base: &serde_json::Map<String, serde_json::Value>,
            candidate: &mut Vec<(String, Vec<CompilerOption>, usize)>,
            i: usize,
        ) {
            debug_assert!(!candidate.is_empty());
            for j in i..candidate.len() {
                let mut hit = String::new();
                for (field, options, index) in candidate.iter() {
                    let option = &options[*index];
                    let mut json = base.clone();
                    let json_value = match option {
                        CompilerOption::Null => {
                            hit.push_str(&format!("({field}=null)"));
                            serde_json::Value::Null
                        }
                        CompilerOption::Bool(b) => {
                            hit.push_str(&format!("({field}={b})"));
                            serde_json::Value::Bool(*b)
                        }
                        CompilerOption::String(s) => {
                            hit.push_str(&format!("({field}={s})"));
                            serde_json::Value::String(s.clone())
                        }
                        CompilerOption::StringArray(arr) => {
                            hit.push_str(&format!("({field}={arr:?})"));
                            serde_json::Value::Array(
                                arr.iter()
                                    .map(|s| serde_json::Value::String(s.clone()))
                                    .collect(),
                            )
                        }
                        CompilerOption::Multiple(_) => unreachable!(),
                    };
                    json.insert(field.clone(), json_value);
                    result.push((json, Some(hit.clone())));
                }
                if i == j {
                    let item = &candidate[i];
                    let bound = item.1.len();
                    if item.2 == bound - 1 {
                        continue;
                    } else {
                        candidate[i].2 += 1;
                    }
                    dfs(result, base, candidate, i);
                } else {
                    dfs(result, base, candidate, j);
                }
            }
        }

        let mut cross_product_candidate = vec![];
        let mut map = serde_json::Map::new();
        for (key, value) in &self.0 {
            let json_value = match value {
                CompilerOption::Null => serde_json::Value::Null,
                CompilerOption::Bool(b) => serde_json::Value::Bool(*b),
                CompilerOption::String(s) => serde_json::Value::String(s.clone()),
                CompilerOption::StringArray(arr) => serde_json::Value::Array(
                    arr.iter()
                        .map(|s| serde_json::Value::String(s.clone()))
                        .collect(),
                ),
                CompilerOption::Multiple(n) => {
                    cross_product_candidate.push((key.to_string(), n.clone(), 0));
                    continue;
                }
            };
            map.insert(key.clone(), json_value);
        }
        if cross_product_candidate.is_empty() {
            vec![(map, None)]
        } else {
            let mut result = Vec::new();
            dfs(&mut result, &map, &mut cross_product_candidate, 0);
            result
        }
    }
}

impl std::ops::Index<&'static str> for CompilerOptions {
    type Output = CompilerOption;

    fn index(&self, index: &'static str) -> &Self::Output {
        self.0.get(index).expect("Key not found")
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum CompilerOption {
    #[default]
    Null,
    Bool(bool),
    String(String),
    StringArray(Vec<String>),
    Multiple(Vec<CompilerOption>),
}

#[test]
fn test_to_serde_json0() {
    let options = CompilerOptions(std::collections::HashMap::from_iter(
        [
            ("option1".to_string(), CompilerOption::Bool(true)),
            (
                "option2".to_string(),
                CompilerOption::Multiple(vec![
                    CompilerOption::String("value1".to_string()),
                    CompilerOption::String("value2".to_string()),
                ]),
            ),
        ]
        .into_iter(),
    ));
    let json = options.to_serde_json();
    assert!(json.len() == 2);
    assert_eq!(json[0].1, Some("(option2=value1)".to_string()));
    assert_eq!(json[0].0["option1"], serde_json::Value::Bool(true));
    assert_eq!(
        json[0].0["option2"],
        serde_json::Value::String("value1".to_string())
    );
    assert_eq!(json[1].1, Some("(option2=value2)".to_string()));
    assert_eq!(json[1].0["option1"], serde_json::Value::Bool(true));
    assert_eq!(
        json[1].0["option2"],
        serde_json::Value::String("value2".to_string())
    );
}
