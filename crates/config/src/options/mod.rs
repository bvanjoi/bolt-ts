#[derive(Debug, Clone, Default, PartialEq)]
pub enum OutDir {
    #[default]
    OwnRoot,
    Custom(String),
}

#[derive(Debug, Clone, Default, PartialEq, serde::Deserialize, serde::Serialize)]
pub enum RawTarget {
    #[default]
    #[serde(rename = "es5")]
    ES5,
    #[serde(rename = "es6")]
    ES6,
    #[serde(rename = "es2015")]
    ES2015,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum Target {
    #[default]
    ES5,
    ES2015,
}

impl Into<Target> for RawTarget {
    fn into(self) -> Target {
        match self {
            RawTarget::ES5 => Target::ES5,
            RawTarget::ES2015 | RawTarget::ES6 => Target::ES2015,
        }
    }
}
