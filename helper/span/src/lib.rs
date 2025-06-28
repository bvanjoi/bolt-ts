use std::sync::Arc;

use bolt_ts_atom::AtomId;
use bolt_ts_fs::CachedFileSystem;

bolt_ts_utils::index!(ModuleID);

impl ModuleID {
    pub const TRANSIENT: ModuleID = ModuleID(u32::MAX);
    pub const DEFAULT: ModuleID = ModuleID(u32::MAX - 1);
}

impl Default for ModuleID {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[derive(Debug, Clone, Copy, Default)]
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
        write!(f, "{}:{}:{}", self.module.as_u32(), self.lo, self.hi)
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    pub id: ModuleID,
    pub is_default_lib: bool,
}

pub type ModulePath = std::path::PathBuf;

pub struct ModuleArena {
    path_map: Vec<ModulePath>,
    content_map: Vec<Arc<String>>,
    modules: Vec<Module>,
}

impl ModuleArena {
    pub fn new(cap: usize) -> Self {
        Self {
            path_map: Vec::with_capacity(cap),
            content_map: Vec::with_capacity(cap),
            modules: Vec::with_capacity(cap),
        }
    }

    pub fn new_module(
        &mut self,
        p: ModulePath,
        is_default_lib: bool,
        fs: &mut impl CachedFileSystem,
        atoms: &mut bolt_ts_atom::AtomMap<'_>,
    ) -> ModuleID {
        let id = ModuleID(self.modules.len() as u32);
        let m = Module { id, is_default_lib };
        self.modules.push(m);
        assert_eq!(id.as_usize(), self.content_map.len());
        let Ok(atom) = fs.read_file(p.as_ref(), atoms) else {
            panic!("File not found: {p:?}");
        };
        // TODO: remove this clone
        let data = atoms.get(atom).to_string();
        self.content_map.push(Arc::new(data));
        assert_eq!(id.as_usize(), self.path_map.len());
        self.path_map.push(p);
        id
    }

    pub fn new_module_with_content(
        &mut self,
        p: ModulePath,
        is_default_lib: bool,
        content: AtomId,
        atoms: &bolt_ts_atom::AtomMap<'_>,
    ) -> ModuleID {
        let id = ModuleID(self.modules.len() as u32);
        let m = Module { id, is_default_lib };
        self.modules.push(m);
        assert_eq!(id.as_usize(), self.content_map.len());
        // TODO: remove this clone
        let data = atoms.get(content).to_string();
        self.content_map.push(Arc::new(data));
        assert_eq!(id.as_usize(), self.path_map.len());
        self.path_map.push(p);
        id
    }

    pub fn get_path(&self, id: ModuleID) -> &ModulePath {
        let idx = id.as_usize();
        assert!(idx < self.path_map.len());
        unsafe { self.path_map.get_unchecked(idx) }
    }
    pub fn get_content(&self, id: ModuleID) -> &Arc<String> {
        let idx = id.as_usize();
        assert!(id.as_usize() < self.content_map.len());
        unsafe { self.content_map.get_unchecked(idx) }
    }
    pub fn get_module(&self, id: ModuleID) -> &Module {
        let idx = id.as_usize();
        assert!(idx < self.modules.len());
        unsafe { self.modules.get_unchecked(idx) }
    }
    pub fn modules(&self) -> &[Module] {
        &self.modules
    }
}
