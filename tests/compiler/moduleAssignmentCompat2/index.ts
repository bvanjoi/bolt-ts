// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleAssignmentCompat2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace A {
    export class C { }
}
namespace B {
    export class C { }
    export class D { }
}

var a: A;
//~^ ERROR: Cannot find name 'A'.
var b: B;
//~^ ERROR: Cannot find name 'B'.

a = b;
b = a; // error