use std::sync::Arc;

use bolt_ts_atom::AtomId;
use bolt_ts_fs::CachedFileSystem;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ModuleID(u32);
impl ModuleID {
    pub const TRANSIENT: ModuleID = ModuleID(u32::MAX);
    pub const DEFAULT: ModuleID = ModuleID(u32::MAX - 1);

    #[inline(always)]
    pub const fn as_u32(&self) -> u32 {
        self.0
    }
    #[inline(always)]
    pub const fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl Default for ModuleID {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Span {
    lo: u32,
    hi: u32,
    module: ModuleID,
}

impl Span {
    #[track_caller]
    #[inline(always)]
    pub fn new(lo: u32, hi: u32, module: ModuleID) -> Self {
        debug_assert!(lo <= hi, "Invalid span: {}..{}", lo, hi);
        Self { lo, hi, module }
    }

    #[inline(always)]
    pub fn lo(&self) -> u32 {
        self.lo
    }

    #[inline(always)]
    pub fn hi(&self) -> u32 {
        self.hi
    }

    #[inline(always)]
    pub fn module(&self) -> ModuleID {
        self.module
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
    id: ModuleID,
    is_default_lib: bool,
}

impl Module {
    #[inline(always)]
    pub fn id(&self) -> ModuleID {
        self.id
    }
    #[inline(always)]
    pub fn is_default_lib(&self) -> bool {
        self.is_default_lib
    }
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
