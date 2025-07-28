mod f64_represent;

pub use f64_represent::{F64Represent, F64RepresentKind};

pub type Diag = Box<dyn bolt_ts_errors::diag_ext::DiagnosticExt + Send + Sync + 'static>;
