// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleWithNoValuesAsType.ts`, Apache-2.0 License

//@compiler-options: target=esnext

namespace A { }
var a: A; // error
//~^ ERROR: Cannot find name 'A'.
namespace B {
    interface I {}
}
var b: B; // error
//~^ ERROR: Cannot find name 'B'.
namespace C {
    namespace M {
        interface I {}
    }
}

var c: C; // error
//~^ ERROR: Cannot find name 'C'.
