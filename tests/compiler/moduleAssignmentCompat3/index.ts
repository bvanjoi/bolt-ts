// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleAssignmentCompat3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace A {
    export var x = 1;
}
namespace B {
    export var x = "";
}

var a: A;
//~^ ERROR: Cannot find name 'A'.
var b: B;
//~^ ERROR: Cannot find name 'B'.

// both errors
a = b;
b = a;
