// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/cloduleWithDuplicateMember2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
    set x(y) { }
    static set y(z) { }
}

namespace C {
    export var x = 1;
}
namespace C {
    export function x() { }
    //~^ ERROR: Duplicate identifier 'x'.
}