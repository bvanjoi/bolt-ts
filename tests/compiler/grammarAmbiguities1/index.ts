// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/grammarAmbiguities1.ts`, Apache-2.0 License

class A { foo() { } }
class B { bar() { }}
function f(x) { return x; }
function g<T, U>(x) { return f(x); }
g<A, B>(7)

f(g<A, B>(7));
f(g < A, B > 7);
//~^ ERROR: Expected 1 arguments, but got 2.
f(g < A, B > +(7));
//~^ ERROR: Expected 1 arguments, but got 2.


g < A;
//~^ ERROR: Operator '<' cannot be applied to types '(x: any) => any' and 'typeof A'.
B > 7;
//~^ ERROR: Operator '>' cannot be applied to types 'typeof B' and 'number'.
