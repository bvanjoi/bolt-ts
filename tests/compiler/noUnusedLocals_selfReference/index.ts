// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noUnusedLocals_selfReference.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

export {}; // Make this a module scope, so these are local variables.

function f() {
  //~^ ERROR: 'f' is declared but its value is never read.
    f;
    function g() {
  //~^ ERROR: 'g' is declared but its value is never read.
        g;
    }
}
class C {
  //~^ ERROR: 'C' is declared but never used.
    m() { C; }
}
enum E { A = 0, B = E.A }
  //~^ ERROR: 'E' is declared but never used.
interface I { x: I };
  //~^ ERROR: 'I' is declared but never used.
type T = { x: T };
  //~^ ERROR: 'T' is declared but never used.
namespace N { N; }
  //~^ ERROR: 'N' is declared but its value is never read.

// Avoid a false positive.
// Previously `T` was considered unused due to merging with the property,
// back when all non-blocks were checked for recursion.
export interface A<T> { T: T }

class P { private m() { this.m; } }
  //~^ ERROR: 'm' is declared but its value is never read.
P;

// Does not detect mutual recursion.
function g() { D; }
class D { m() { g; } }
