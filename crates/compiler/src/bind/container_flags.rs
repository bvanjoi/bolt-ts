bitflags::bitflags! {
  #[derive(Clone, Copy, Debug, Default)]
  pub struct ContainerFlags: u8 {
    const IS_CONTAINER = 1 << 0;
    const IS_BLOCK_SCOPED_CONTAINER = 1 << 1;
    const IS_CONTROL_FLOW_CONTAINER = 1 << 2;
    const IS_FUNCTION_LIKE = 1 << 3;
    const IS_FUNCTION_EXPRESSION = 1 << 4;
    const HAS_LOCALS = 1 << 5;
    const IS_INTERFACE = 1 << 6;
    const IS_OBJECT_LITERAL_OR_CLASS_EXPRESSION_METHOD_OR_ACCESSOR = 1 << 7;
  }
}
