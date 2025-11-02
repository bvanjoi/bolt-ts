bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct NodeFlags: u32 {
        const LET                                         = 1 << 0;
        const CONST                                       = 1 << 1;
        const USING                                       = 1 << 2;
        const NESTED_NAMESPACE                            = 1 << 3;
        const SYNTHESIZED                                 = 1 << 4;
        const NAMESPACE                                   = 1 << 5;
        const OPTIONAL_CHAIN                              = 1 << 6;
        const EXPORT_CONTEXT                              = 1 << 7;
        const CONTAINS_THIS                               = 1 << 8;
        const HAS_IMPLICIT_RETURN                         = 1 << 9;
        const HAS_EXPLICIT_RETURN                         = 1 << 10;
        const GLOBAL_AUGMENTATION                         = 1 << 11;
        const HAS_ASYNC_FUNCTIONS                         = 1 << 12;
        const AWAIT_CONTEXT                               = 1 << 16;
        const THIS_NODE_HAS_ERROR                         = 1 << 18;
        const JAVASCRIPT_FILE                             = 1 << 19;
        const THIS_NODE_OR_ANY_SUB_NODES_HAS_ERROR        = 1 << 20;
        const HAS_AGGREGATED_CHILD_DATA                   = 1 << 21;
        const POSSIBLY_CONTAINS_DYNAMIC_IMPORT            = 1 << 22;
        const POSSIBLY_CONTAINS_IMPORT_META               = 1 << 23;
        const JSDOC                                       = 1 << 24;
        const AMBIENT                                     = 1 << 25;
        const IN_WITH_STATEMENT                           = 1 << 26;
        const JSON_FILE                                   = 1 << 27;
        const TYPE_CACHED                                 = 1 << 28;
        const DEPRECATED                                  = 1 << 29;

        const AWAIT_USING                                 = Self::CONST.bits()
                                                            | Self::USING.bits();
        const CONSTANT                                    = Self::CONST.bits()
                                                            | Self::USING.bits();
        const BLOCK_SCOPED                                = Self::LET.bits()
                                                            | Self::CONST.bits()
                                                            | Self::USING.bits();

        const REACHABILITY_CHECK_FLAGS                    = Self::HAS_IMPLICIT_RETURN.bits()
                                                            | Self::HAS_EXPLICIT_RETURN.bits();
        const REACHABILITY_AND_EMIT_FLAGS                 = Self::REACHABILITY_CHECK_FLAGS.bits()
                                                            | Self::HAS_ASYNC_FUNCTIONS.bits();

        const IDENTIFIER_IS_IN_JS_DOC_NAMESPACE           = Self::HAS_ASYNC_FUNCTIONS.bits();
        const IDENTIFIER_HAS_EXTENDED_UNICODE_ESCAPE      = Self::CONTAINS_THIS.bits();
        const PERMANENTLY_SET_INCREMENTAL_FLAGS           = Self::POSSIBLY_CONTAINS_DYNAMIC_IMPORT.bits()
                                                            | Self::POSSIBLY_CONTAINS_IMPORT_META.bits();

    }



}
