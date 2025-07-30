bitflags::bitflags! {
  #[derive(Clone, Copy, Debug)]
  pub struct NodeCheckFlags: u32 {
      const TYPE_CHECKED                                      = 1 << 0;
      const LEXICAL_THIS                                      = 1 << 1;
      const CAPTURE_THIS                                      = 1 << 2;
      const CAPTURE_NEW_TARGET                                = 1 << 3;
      const SUPER_INSTANCE                                    = 1 << 4;
      const SUPER_STATIC                                      = 1 << 5;
      const CONTEXT_CHECKED                                   = 1 << 6;
      const METHOD_WITH_SUPER_PROPERTY_ACCESS_IN_ASYNC        = 1 << 7;
      const METHOD_WITH_SUPER_PROPERTY_ASSIGNMENT_IN_ASYNC    = 1 << 8;
      const CAPTURE_ARGUMENTS                                 = 1 << 9;
      const ENUM_VALUES_COMPUTED                              = 1 << 10;
      const LEXICAL_MODULE_MERGES_WITH_CLASS                  = 1 << 11;
      const LOOP_WITH_CAPTURED_BLOCK_SCOPED_BINDING           = 1 << 12;
      const CONTAINS_CAPTURED_BLOCK_SCOPE_BINDING             = 1 << 13;
      const CAPTURED_BLOCK_SCOPED_BINDING                     = 1 << 14;
      const BLOCK_SCOPED_BINDING_IN_LOOP                      = 1 << 15;
      const NEEDS_LOOP_OUT_PARAMETER                          = 1 << 16;
      const ASSIGNMENTS_MARKED                                = 1 << 17;
      const CONTAINS_CONSTRUCTOR_REFERENCE                    = 1 << 18;
      const CONSTRUCTOR_REFERENCE                             = 1 << 19;
      const CONTAINS_CLASS_WITH_PRIVATE_IDENTIFIERS           = 1 << 20;
      const CONTAINS_SUPER_PROPERTY_IN_STATIC_INITIALIZER     = 1 << 21;
      const IN_CHECK_IDENTIFIER                               = 1 << 22;
      const PARTIALLY_TYPE_CHECKED                            = 1 << 23;

      const LAZY_FLAGS = Self::SUPER_INSTANCE.bits()
          | Self::SUPER_STATIC.bits()
          | Self::METHOD_WITH_SUPER_PROPERTY_ACCESS_IN_ASYNC.bits()
          | Self::METHOD_WITH_SUPER_PROPERTY_ASSIGNMENT_IN_ASYNC.bits()
          | Self::CONTAINS_SUPER_PROPERTY_IN_STATIC_INITIALIZER.bits()
          | Self::CAPTURE_ARGUMENTS.bits()
          | Self::CONTAINS_CAPTURED_BLOCK_SCOPE_BINDING.bits()
          | Self::NEEDS_LOOP_OUT_PARAMETER.bits()
          | Self::CONTAINS_CONSTRUCTOR_REFERENCE.bits()
          | Self::CONSTRUCTOR_REFERENCE.bits()
          | Self::CAPTURED_BLOCK_SCOPED_BINDING.bits()
          | Self::BLOCK_SCOPED_BINDING_IN_LOOP.bits()
          | Self::LOOP_WITH_CAPTURED_BLOCK_SCOPED_BINDING.bits();
  }
}
