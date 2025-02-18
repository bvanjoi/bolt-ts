bitflags::bitflags! {
  #[derive(Clone, Copy, Debug, Default)]
  pub struct NodeFlags: u32 {
    const NESTED_NAMESPACE                    = 1 << 3;
    const OPTIONAL_CHAIN                      = 1 << 6;
    const DISALLOW_IN_CONTEXT                 = 1 << 13;
    const HAS_ASYNC_FUNCTIONS                 = 1 << 12;
    const DECORATOR_CONTEXT                   = 1 << 15;
    const DISALLOW_CONDITIONAL_TYPES_CONTEXT  = 1 << 17;
    const AMBIENT                             = 1 << 25;

    const DISALLOW_IN_AND_DECORATOR_CONTEXT   = Self::DISALLOW_IN_CONTEXT.bits() | Self::DECORATOR_CONTEXT.bits();
    const IDENTIFIER_IS_IN_JS_DOC_NAMESPACE   = Self::HAS_ASYNC_FUNCTIONS.bits();
  }
}
