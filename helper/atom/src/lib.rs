use bolt_ts_utils::FxIndexSet;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct Atom(u32);

impl Atom {
    #[inline]
    pub const fn new(id: u32) -> Self {
        Atom(id)
    }

    #[inline]
    pub const fn as_u32(&self) -> u32 {
        self.0
    }
}

impl nohash_hasher::IsEnabled for Atom {}

#[derive(Debug)]
pub struct AtomIntern {
    arena: Vec<String>,
    set: FxIndexSet<&'static str>,
}

impl AtomIntern {
    pub fn prefill(list: &[&'static str]) -> Self {
        let set = FxIndexSet::from_iter(list.iter().cloned());
        Self {
            set,
            arena: Vec::new(),
        }
    }

    pub fn get(&self, atom: Atom) -> &'static str {
        self.set[atom.0 as usize]
    }

    pub fn atom(&mut self, s: &str) -> Atom {
        if let Some(index) = self.set.get_index_of(s) {
            return Atom(index as u32);
        }

        self.arena.push(s.to_string());
        let s = self.arena.last().unwrap();
        let s: &'static str = unsafe { &*(s.as_str() as *const str) };
        let (idx, prev_is_not_exist) = self.set.insert_full(s);
        debug_assert!(prev_is_not_exist);
        Atom(idx as u32)
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
        pub fn $prefilled_atom_fn_name() -> bolt_ts_atom::AtomIntern {
            bolt_ts_atom::AtomIntern::prefill(PREFILLED)
        }

    };
    ( $owner: ident, $(($name:ident, [$lit:literal, $idx: literal])),* $(,)? ) => {
        paste::paste! {
            $(pub const [<$name _STR>]: &str = $lit;)*
            $(pub const $name: Atom = Atom::new($idx);)*
        }
        pub const $owner: &[(&str, Atom)] = &[$(($lit, $name),)*];
    }
}
