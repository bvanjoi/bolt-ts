// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeValueConflict2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M1 {
    export class A<T> {
        constructor(a: T) {
        }
    }
}
namespace M2 {
    var M1 = 0;
    // Should error.  M1 should bind to the variable, not to the module.
    class B extends M1.A<string> {
      //~^ ERROR: Property 'A' does not exist on type 'number'.
    }
}
namespace M3 {
    // Shouldn't error
    class B extends M1.A<string> {
    }
}