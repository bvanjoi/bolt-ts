pub trait EmitHelper {
    fn name() -> &'static str;
    fn text() -> &'static str;
}

pub struct ExtendsHelper;
impl EmitHelper for ExtendsHelper {
    fn text() -> &'static str {
        include_str!("./helper/__extends.js")
    }
    fn name() -> &'static str {
        "__extends"
    }
}

bitflags::bitflags! {
    #[derive(Default)]
    pub struct EmitHelperFlags: u8 {
        const EXTENDS = 1 << 0;
    }
}

pub const EMIT_HELPER_LIST: &[EmitHelperFlags] = &[EmitHelperFlags::EXTENDS];
