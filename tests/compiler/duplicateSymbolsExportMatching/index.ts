// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateSymbolsExportMatching.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

namespace M {
    export interface E { }
    interface I { }
}
namespace M {
    export interface E { } // ok
    interface I { } // ok
}

// Doesn't match export visibility, but it's in a different parent, so it's ok
namespace M {
    interface E { } // ok
    export interface I { } // ok
}

namespace N {
    interface I { }
    interface I { } // ok
    export interface E { }
    export interface E { } // ok
}

namespace N2 {
    interface I { }
    //~^ ERROR: Individual declarations in merged declaration 'I' must be all exported or all local.
    export interface I { } // error
    //~^ ERROR: Individual declarations in merged declaration 'I' must be all exported or all local.
    export interface E { }
    //~^ ERROR: Individual declarations in merged declaration 'E' must be all exported or all local.
    interface E { } // error
    //~^ ERROR: Individual declarations in merged declaration 'E' must be all exported or all local.
}

// Should report error only once for instantiated module
namespace M {
    namespace inst {
        //~^ ERROR: Individual declarations in merged declaration 'inst' must be all exported or all local.
        var t;
    }
    export namespace inst { // one error
        //~^ ERROR: Individual declarations in merged declaration 'inst' must be all exported or all local.
        var t;
    }
}

// Variables of the same / different type
namespace M2 {
    var v: string;
    //~^ ERROR: Individual declarations in merged declaration 'v' must be all exported or all local.
    export var v: string; // one error (visibility)
    //~^ ERROR: Individual declarations in merged declaration 'v' must be all exported or all local.
    var w: number;
    //~^ ERROR: Individual declarations in merged declaration 'w' must be all exported or all local.
    export var w: string; // two errors (visibility and type mismatch)
    //~^ ERROR: Individual declarations in merged declaration 'w' must be all exported or all local.
}

namespace M {
    namespace F {
        //~^ ERROR: Individual declarations in merged declaration 'F' must be all exported or all local.
        //~| ERROR: A namespace declaration cannot be located prior to a class or function with which it is merged.
        var t;
    }
    export function F() { } // Only one error for duplicate identifier (don't consider visibility)
        //~^ ERROR: Individual declarations in merged declaration 'F' must be all exported or all local.
}

namespace M {
    class C { }
    //~^ ERROR: Individual declarations in merged declaration 'C' must be all exported or all local.
    namespace C { }
    //~^ ERROR: Individual declarations in merged declaration 'C' must be all exported or all local.
    export namespace C { // Two visibility errors (one for the clodule symbol, and one for the merged container symbol)
    //~^ ERROR: Individual declarations in merged declaration 'C' must be all exported or all local.
        var t;
    }
}

// Top level
interface D { }
//~^ ERROR: Individual declarations in merged declaration 'D' must be all exported or all local.
export interface D { }
//~^ ERROR: Individual declarations in merged declaration 'D' must be all exported or all local.