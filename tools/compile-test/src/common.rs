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

    pub fn to_serde_json(&self) -> Vec<serde_json::Map<String, serde_json::Value>> {
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
                CompilerOption::Multiple(_) => todo!(),
            };
            map.insert(key.clone(), json_value);
        }
        vec![map]
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
