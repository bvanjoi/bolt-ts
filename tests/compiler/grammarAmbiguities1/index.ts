// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/grammarAmbiguities1.ts`, Apache-2.0 License

class A { foo() { } }
class B { bar() { }}
function f(x) { return x; }
//~^ ERROR: Parameter 'x' implicitly has an 'any' type.
function g<T, U>(x) { return f(x); }
//~^ ERROR: Parameter 'x' implicitly has an 'any' type.
g<A, B>(7)

f(g<A, B>(7));
f(g < A, B > 7);
//~^ ERROR: Expected 1 arguments, but got 2.
//~| ERROR: Operator '<' cannot be applied to types '<T, U>(x: any) => any' and 'typeof A'.
//~| ERROR: Operator '>' cannot be applied to types 'typeof B' and 'number'.
f(g < A, B > +(7));
//~^ ERROR: Expected 1 arguments, but got 2.
//~| ERROR: Operator '<' cannot be applied to types '<T, U>(x: any) => any' and 'typeof A'.
//~| ERROR: Operator '>' cannot be applied to types 'typeof B' and 'number'.


g < A;
//~^ ERROR: Operator '<' cannot be applied to types '<T, U>(x: any) => any' and 'typeof A'.
B > 7;
//~^ ERROR: Operator '>' cannot be applied to types 'typeof B' and 'number'.
