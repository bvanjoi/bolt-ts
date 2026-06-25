// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentToReferenceTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
}
M = null;
//~^ ERROR: Cannot find name 'M'.

class C {
}
C = null;
//~^ ERROR: Cannot assign to 'C' because it is a class.

enum E {
}
E = null;
//~^ ERROR: Cannot assign to 'E' because it is a enum.

function f() { }
f = null;
//~^ ERROR: Cannot assign to 'f' because it is a function.

var x = 1;
x = null;

function g(x) {
    x = null;
}