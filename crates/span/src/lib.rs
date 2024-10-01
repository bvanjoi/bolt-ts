mod sys;

use std::sync::Arc;

use rustc_hash::FxHashMap;

#[macro_export]
macro_rules! new_index {
    ($name: ident) => {
        #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
        pub struct $name(u32);
        impl $name {
            pub fn root() -> $name {
                $name(0)
            }
            pub fn next(&self) -> $name {
                $name(self.0 + 1)
            }
            pub fn as_u32(&self) -> u32 {
                self.0
            }
        }
    };
}

crate::new_index!(ModuleID);

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

pub struct Module {
    pub id: ModuleID,
}

pub enum ModulePath {
    Real(std::path::PathBuf),
    Virtual,
}

pub struct ModuleArena {
    pub path_map: FxHashMap<ModuleID, ModulePath>,
    pub content_map: FxHashMap<ModuleID, Arc<String>>,
    next_module_id: ModuleID,
}

impl ModuleArena {
    pub fn new() -> Self {
        Self {
            path_map: Default::default(),
            content_map: Default::default(),
            next_module_id: ModuleID::root(),
        }
    }
    fn next_module_id(&mut self) -> ModuleID {
        let old = self.next_module_id;
        self.next_module_id = self.next_module_id.next();
        old
    }

    pub fn new_module(&mut self, p: ModulePath) -> Module {
        let id = self.next_module_id();
        let m = Module { id };
        if let ModulePath::Real(p) = &p {
            let prev = self
                .content_map
                .insert(id, Arc::new(sys::read_file_with_encoding(p).unwrap()));
            assert!(prev.is_none())
        };
        let prev = self.path_map.insert(id, p);
        assert!(prev.is_none());
        m
    }
}
