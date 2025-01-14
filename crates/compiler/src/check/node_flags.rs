bitflags::bitflags! {
  #[derive(Clone, Copy, Debug)]
  pub struct NodeFlags: u32 {
      const TYPE_CHECKED = 1 << 0;   // Node has been type checked
      const LEXICAL_THIS = 1 << 1;   // Lexical 'this' reference
      const CAPTURE_THIS = 1 << 2;   // Lexical 'this' used in body
      const CAPTURE_NEW_TARGET = 1 << 3;   // Lexical 'new.target' used in body
      const SUPER_INSTANCE = 1 << 4;   // Instance 'super' reference
      const SUPER_STATIC = 1 << 5;   // Static 'super' reference
      const CONTEXT_CHECKED = 1 << 6;   // Contextual types have been assigned
      const METHOD_WITH_SUPER_PROPERTY_ACCESS_IN_ASYNC = 1 << 7;   // A method that contains a SuperProperty access in an async context.
      const METHOD_WITH_SUPER_PROPERTY_ASSIGNMENT_IN_ASYNC = 1 << 8;   // A method that contains a SuperProperty assignment in an async context.
      const CAPTURE_ARGUMENTS = 1 << 9;   // Lexical 'arguments' used in body
      const ENUM_VALUES_COMPUTED = 1 << 10;  // Values for enum members have been computed, and any errors have been reported for them.
      const LEXICAL_MODULE_MERGES_WITH_CLASS = 1 << 11;  // Instantiated lexical module declaration is merged with a previous class declaration.
      const LOOP_WITH_CAPTURED_BLOCK_SCOPED_BINDING = 1 << 12;  // Loop that contains block scoped variable captured in closure
      const CONTAINS_CAPTURED_BLOCK_SCOPE_BINDING = 1 << 13;  // Part of a loop that contains block scoped variable captured in closure
      const CAPTURED_BLOCK_SCOPED_BINDING = 1 << 14;  // Block-scoped binding that is captured in some function
      const BLOCK_SCOPED_BINDING_IN_LOOP = 1 << 15;  // Block-scoped binding with declaration nested inside iteration statement
      const NEEDS_LOOP_OUT_PARAMETER = 1 << 16;  // Block scoped binding whose value should be explicitly copied outside of the converted loop
      const ASSIGNMENTS_MARKED = 1 << 17;  // Parameter assignments have been marked
      const CONTAINS_CONSTRUCTOR_REFERENCE = 1 << 18;  // Class or class element that contains a binding that references the class constructor.
      const CONSTRUCTOR_REFERENCE = 1 << 19;  // Binding to a class constructor inside of the class's body.
      const CONTAINS_CLASS_WITH_PRIVATE_IDENTIFIERS = 1 << 20;  // Marked on all block-scoped containers containing a class with private identifiers.
      const CONTAINS_SUPER_PROPERTY_IN_STATIC_INITIALIZER = 1 << 21;  // Marked on all block-scoped containers containing a static initializer with 'super.x' or 'super[x]'.
      const IN_CHECK_IDENTIFIER = 1 << 22;
      const PARTIALLY_TYPE_CHECKED = 1 << 23;  // Node has been partially type checked

      /** These flags are LazyNodeCheckFlags and can be calculated lazily by `hasNodeCheckFlag` */
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
