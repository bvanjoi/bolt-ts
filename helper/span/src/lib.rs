use bolt_ts_atom::Atom;

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

#[derive(Clone, Debug, Copy)]
pub struct Module {
    id: ModuleID,
    // TODO: delete `is_default_lib`, and use `is_preserve` instead.
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

#[derive(Default)]
pub struct ModuleArena {
    path_map: Vec<ModulePath>,
    content_map: Vec<String>,
    modules: Vec<Module>,

    preserve_bound: usize,
    preserve_index: usize,
}

impl ModuleArena {
    pub fn preserve(cap: usize) -> Self {
        let fallback_module = Module {
            id: ModuleID::DEFAULT,
            is_default_lib: true,
        };
        Self {
            path_map: vec![ModulePath::new(); cap],
            content_map: vec![String::new(); cap],
            modules: vec![fallback_module; cap],
            preserve_bound: cap,
            preserve_index: 0,
        }
    }

    fn next_preserve_index(&mut self) -> usize {
        debug_assert!(self.preserve_index < self.preserve_bound);
        let index = self.preserve_index;
        self.preserve_index += 1;
        index
    }

    pub fn new_module(
        &mut self,
        p: ModulePath,
        is_default_lib: bool,
        read_file: impl FnOnce(&std::path::Path, &mut bolt_ts_atom::AtomIntern) -> Option<Atom>,
        atoms: &mut bolt_ts_atom::AtomIntern,
    ) -> ModuleID {
        let id = ModuleID(self.modules.len() as u32);
        let m = Module { id, is_default_lib };
        self.modules.push(m);
        assert_eq!(id.as_usize(), self.content_map.len());
        // TODO: dont use atom when read file.
        let Some(atom) = read_file(p.as_path(), atoms) else {
            panic!("File not found: {p:?}");
        };
        let data = atoms.get(atom).to_string();
        self.content_map.push(data);
        assert_eq!(id.as_usize(), self.path_map.len());
        self.path_map.push(p);
        id
    }

    pub fn new_module_within_preserve(
        &mut self,
        p: ModulePath,
        is_default_lib: bool,
        read_file: impl FnOnce(&std::path::Path, &mut bolt_ts_atom::AtomIntern) -> Option<Atom>,
        atoms: &mut bolt_ts_atom::AtomIntern,
    ) -> ModuleID {
        let index = self.next_preserve_index();
        let id = ModuleID(index as u32);
        let m = Module { id, is_default_lib };
        self.modules[index] = m;
        // TODO: don't use atom when read file.
        let Some(atom) = read_file(p.as_path(), atoms) else {
            panic!("File not found: {p:?}");
        };
        let data = atoms.get(atom).to_string();
        self.content_map[index] = data;
        self.path_map[index] = p;
        id
    }

    pub fn new_module_with_content(
        &mut self,
        p: ModulePath,
        is_default_lib: bool,
        content: Atom,
        atoms: &bolt_ts_atom::AtomIntern,
    ) -> ModuleID {
        let id = ModuleID(self.modules.len() as u32);
        let m = Module { id, is_default_lib };
        self.modules.push(m);
        assert_eq!(id.as_usize(), self.content_map.len());
        // TODO: remove this clone
        let data = atoms.get(content).to_string();
        self.content_map.push(data);
        assert_eq!(id.as_usize(), self.path_map.len());
        self.path_map.push(p);
        id
    }

    pub fn new_module_with_content_within_preserve(
        &mut self,
        p: ModulePath,
        is_default_lib: bool,
        content: Atom,
        atoms: &bolt_ts_atom::AtomIntern,
    ) -> ModuleID {
        let index = self.next_preserve_index();
        let id = ModuleID(index as u32);
        let m = Module { id, is_default_lib };
        self.modules[index] = m;
        // TODO: remove this clone
        let data = atoms.get(content).to_string();
        self.content_map[index] = data;
        self.path_map[index] = p;
        id
    }

    pub fn get_path(&self, id: ModuleID) -> &ModulePath {
        let idx = id.as_usize();
        assert!(idx < self.path_map.len());
        unsafe { self.path_map.get_unchecked(idx) }
    }

    pub fn get_content(&self, id: ModuleID) -> &str {
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

    pub fn is_preserve(&self, id: ModuleID) -> bool {
        id.as_usize() < self.preserve_bound
    }
}
