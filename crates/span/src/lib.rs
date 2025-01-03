use std::sync::Arc;

#[macro_export]
macro_rules! new_index {
    ($name: ident) => {
        #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
        pub struct $name(u32);
        impl $name {
            pub const fn root() -> $name {
                $name(0)
            }
            pub const fn next(&self) -> $name {
                $name(self.0 + 1)
            }
            pub const fn as_u32(&self) -> u32 {
                self.0
            }
            pub const fn as_usize(&self) -> usize {
                self.0 as usize
            }
        }
    };
}

crate::new_index!(ModuleID);
impl ModuleID {
    pub const MOCK: ModuleID = ModuleID(u32::MAX);
}

#[macro_export]
macro_rules! new_index_with_module {
    ($name: ident) => {
        #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
        pub struct $name {
            module: bolt_ts_span::ModuleID,
            index: u32,
        }
        impl $name {
            pub const fn root(module: bolt_ts_span::ModuleID) -> $name {
                $name { module, index: 0 }
            }
            pub const fn next(&self) -> $name {
                $name {
                    module: self.module,
                    index: self.index + 1,
                }
            }
            pub const fn index_as_u32(&self) -> u32 {
                self.index
            }
            pub const fn index_as_usize(&self) -> usize {
                self.index as usize
            }
            pub const fn module(&self) -> bolt_ts_span::ModuleID {
                self.module
            }
        }
    };
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
    pub deps: Vec<ModuleID>,
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

impl Default for ModuleArena {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn new_module(&mut self, p: ModulePath, global: bool) -> ModuleID {
        let id = ModuleID(self.modules.len() as u32);
        let m = Module {
            id,
            global,
            deps: Vec::with_capacity(32),
        };
        self.modules.push(m);
        if let ModulePath::Real(p) = &p {
            assert!(id.as_usize() == self.content_map.len());
            self.content_map
                .push(Arc::new(bolt_ts_fs::read_file_with_encoding(p).unwrap()));
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
