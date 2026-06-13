// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleAssignmentCompat1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace A {
    export class C { }
}
namespace B {
    export class C { }
    class D { }
}

var a: A;
//~^ ERROR: Cannot find name 'A'.
var b: B;
//~^ ERROR: Cannot find name 'B'.

// no error
a = b;
b = a;

