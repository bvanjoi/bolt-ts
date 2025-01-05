use std::sync::Arc;

use bolt_ts_atom::AtomId;
use bolt_ts_fs::CachedFileSystem;

bolt_ts_utils::index!(ModuleID);

impl ModuleID {
    pub const MOCK: ModuleID = ModuleID(u32::MAX);
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
    pub module: ModuleID,
}

impl Span {
    pub fn new(lo: u32, hi: u32, module: ModuleID) -> Self {
        Self { lo, hi, module }
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(value: Span) -> Self {
        let len = value.hi - value.lo;
        (value.lo as usize, len as usize).into()
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.lo, self.hi)
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    pub id: ModuleID,
    pub global: bool,
}

pub enum ModulePath {
    Real(std::path::PathBuf),
    Virtual,
}

pub struct ModuleArena {
    path_map: Vec<ModulePath>,
    content_map: Vec<Arc<String>>,
    modules: Vec<Module>,
}

impl ModuleArena {
    pub fn new() -> Self {
        let cap = 1024 * 8;
        Self {
            path_map: Vec::with_capacity(cap),
            content_map: Vec::with_capacity(cap),
            modules: Vec::with_capacity(cap),
        }
    }

    pub fn new_module(
        &mut self,
        p: ModulePath,
        global: bool,
        fs: &mut impl CachedFileSystem,
        atoms: &mut bolt_ts_atom::AtomMap<'_>,
    ) -> ModuleID {
        let id = ModuleID(self.modules.len() as u32);
        let m = Module { id, global };
        self.modules.push(m);
        if let ModulePath::Real(p) = &p {
            assert!(id.as_usize() == self.content_map.len());
            let atom = fs.read_file(p.as_ref(), atoms).unwrap();
            let data = atoms.get(atom).to_string();
            self.content_map.push(Arc::new(data));
        };
        assert!(id.as_usize() == self.path_map.len());
        self.path_map.push(p);
        id
    }

    pub fn new_module_with_content(
        &mut self,
        p: ModulePath,
        global: bool,
        content: AtomId,
        atoms: &bolt_ts_atom::AtomMap<'_>,
    ) -> ModuleID {
        let id = ModuleID(self.modules.len() as u32);
        let m = Module { id, global };
        self.modules.push(m);
        if let ModulePath::Real(_) = &p {
            assert!(id.as_usize() == self.content_map.len());
            let data = atoms.get(content).to_string();
            self.content_map.push(Arc::new(data));
        };
        assert!(id.as_usize() == self.path_map.len());
        self.path_map.push(p);
        id
    }

    pub fn get_path(&self, id: ModuleID) -> &ModulePath {
        &self.path_map[id.as_usize()]
    }
    pub fn get_content(&self, id: ModuleID) -> &Arc<String> {
        &self.content_map[id.as_usize()]
    }
    pub fn get_module(&self, id: ModuleID) -> &Module {
        &self.modules[id.as_usize()]
    }
    pub fn modules(&self) -> &[Module] {
        &self.modules
    }
}
