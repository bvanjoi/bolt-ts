// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multiModuleFundule1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function C(x: number) { }

namespace C {
    export var x = 1;
}
namespace C {
    export function foo() { }
}

var r = C(2);
var r2 = new C(2); // using void returning function as constructor
var r3 = C.foo();
const r4: number = C.x;