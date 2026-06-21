// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateIdentifiersAcrossContainerBoundaries.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
    export interface I { }
}
namespace M {
    export class I { }
}

namespace M {
    export function f() { }
    //~^ ERROR: Function with bodies can only merge with classes that are ambient.
}
namespace M {
    export class f { } // error
    //~^ ERROR: Class declaration cannot implement overload list for 'f'.
}

namespace M {
    function g() { }
}
namespace M {
    export class g { } // no error
}

namespace M {
    export class C { }
}
namespace M {
    function C() { } // no error
}

namespace M {
    export var v = 3;
}
namespace M {
    export var v = 3; // error for redeclaring var in a different parent
}

class Foo {
    static x: number;
}

namespace Foo {
    export var x: number; // error for redeclaring var in a different parent
    //~^ ERROR: Duplicate identifier 'x'.
}

namespace N {
    export namespace F {
        var t;
    }
}
declare namespace N {
    export function F(); // no error because function is ambient
}
