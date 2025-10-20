use bolt_ts_optimize::IrOutput;
use indexmap::IndexMap;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(serde::Serialize, Clone)]
pub struct Diag {
    start: (u32, u32),
    end: (u32, u32),
    path: String,
    msg: String,
}

struct CFG {
    entry_graph: usize,
}

type CFGMap = IndexMap<String, bolt_ts_optimize::LoweringResult>;
type OutputFiles = IndexMap<String, String>;

#[wasm_bindgen]
pub struct JsQuery {
    db: bolt_ts_compiler::Output,

    diags: std::cell::OnceCell<Vec<Diag>>,
    output_files: std::cell::OnceCell<OutputFiles>,
    cfg_map: std::cell::OnceCell<CFGMap>,
}

impl JsQuery {
    pub(super) fn new(db: bolt_ts_compiler::Output) -> Self {
        Self {
            db,
            diags: std::cell::OnceCell::new(),
            output_files: std::cell::OnceCell::new(),
            cfg_map: std::cell::OnceCell::new(),
        }
    }
}

#[wasm_bindgen]
impl JsQuery {
    #[wasm_bindgen(js_name = "queryDiag")]
    pub fn query_diag(&self) -> *const Vec<Diag> {
        self.diags.get_or_init(|| {
            self.db
                .diags
                .iter()
                .map(|diag| {
                    let m = diag.inner.module_id();
                    let path = self.db.module_arena.get_path(m);
                    let path = path.to_string_lossy().to_string();
                    let primary_label = diag
                        .inner
                        .labels()
                        .unwrap()
                        .find(|label| label.primary())
                        .expect("at least one primary label");
                    let msg = if let Some(msg) = primary_label.label() {
                        msg.to_string()
                    } else {
                        diag.inner.to_string()
                    };
                    let code = self.db.module_arena.get_content(diag.inner.module_id());
                    let (start, end) =
                        bolt_ts_errors::miette_label_span_to_line_position(primary_label, code);
                    Diag {
                        path,
                        start: (start.line as u32, start.column as u32),
                        end: (end.line as u32, end.column as u32),
                        msg,
                    }
                })
                .collect::<Vec<_>>()
        }) as _
    }

    #[wasm_bindgen(js_name = "queryCFG")]
    pub fn query_cfg(&self) -> *const Vec<(bolt_ts_span::ModuleID, IrOutput)> {
        &self.db.ir as *const _
        // self.cfg_map.get_or_init(|| {
        // self.db.ir
        // .iter()
        // .map(|(m, ir)| {
        //     let path = self.db.module_arena.get_path(*m);
        //     let file_path = path.with_extension("js");
        //     let cfg = CFG {
        //         entry_graph: ir.lowered.entry_graph,
        //     };
        //     (file_path.to_string_lossy().to_string(), cfg)
        // })
        // .collect()
        // })
    }
}
