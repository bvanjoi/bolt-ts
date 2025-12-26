// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/moduleAssignmentCompat4.ts`, Apache-2.0 License

namespace A {
   export namespace M {
        class C { }
    }
}
namespace B {
    export namespace M {
        export class D { }
    }
}

var a: A;
//~^ ERROR: Cannot find name 'A'.
var b: B;
//~^ ERROR: Cannot find name 'B'.

a = b;
b = a; // error
