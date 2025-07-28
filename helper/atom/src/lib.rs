use bolt_ts_utils::FxIndexSet;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct AtomId(u32);

impl AtomId {
    pub const fn new(id: u32) -> Self {
        AtomId(id)
    }
}

impl std::hash::Hash for AtomId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[derive(Debug)]
pub struct AtomMap {
    arena: Vec<String>,
    set: FxIndexSet<&'static str>,
}

impl AtomMap {
    pub fn prefill(list: &[&'static str]) -> Self {
        let set = FxIndexSet::from_iter(list.iter().cloned());
        Self {
            set,
            arena: Vec::new(),
        }
    }

    pub fn get(&self, atom: AtomId) -> &'static str {
        self.set[atom.0 as usize]
    }

    pub fn atom(&mut self, s: &str) -> AtomId {
        if let Some(index) = self.set.get_index_of(s) {
            return AtomId(index as u32);
        }

        self.arena.push(s.to_string());
        let s = self.arena.last().unwrap();
        let s: &'static str = unsafe { &*(s.as_str() as *const str) };
        let (idx, prev_is_not_exist) = self.set.insert_full(s);
        debug_assert!(prev_is_not_exist);
        AtomId(idx as u32)
    }
}

#[macro_export]
macro_rules! prefilled_atom_map {
    (
    $prefilled_atom_fn_name: ident,
    {
        $(
            $owner: ident: {
                $( $name:ident : [$lit:literal, $idx: literal]),* $(,)?
            },
        )+
    }) => {
        $(
            prefilled_atom_map!($owner, $(($name, [$lit, $idx])),*);
        )+

        const PREFILLED: &[&'static str] = &[
            $(
                $( $lit, )*
            )+
        ];
        pub fn $prefilled_atom_fn_name() -> bolt_ts_atom::AtomMap {
            bolt_ts_atom::AtomMap::prefill(PREFILLED)
        }

    };
    ( $owner: ident, $(($name:ident, [$lit:literal, $idx: literal])),* $(,)? ) => {
        paste::paste! {
            $(pub const [<$name _STR>]: &str = $lit;)*
            $(pub const $name: AtomId = AtomId::new($idx);)*
        }
        pub const $owner: &[(&str, AtomId)] = &[$(($lit, $name),)*];
    }
}
