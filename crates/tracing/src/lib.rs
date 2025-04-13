use std::env::{self, VarError};

use tracing_subscriber::filter::{EnvFilter, LevelFilter};
use tracing_subscriber::layer::SubscriberExt;

struct LoggerConfig {
    filter: Result<String, VarError>,
}

impl LoggerConfig {
    pub fn from_env() -> Self {
        const ENV: &str = "BOLT_TS_LOG";
        let filter = env::var(ENV);
        Self { filter }
    }
}

pub fn init_tracing() {
    let cfg = LoggerConfig::from_env();
    let filter = match cfg.filter {
        Ok(filter) => EnvFilter::new(filter),
        _ => return,
    };
    let layer = tracing_tree::HierarchicalLayer::default().with_writer(std::io::stderr);
    let subscribe = tracing_subscriber::Registry::default()
        .with(filter)
        .with(layer);
    tracing::subscriber::set_global_default(subscribe).unwrap();
}
