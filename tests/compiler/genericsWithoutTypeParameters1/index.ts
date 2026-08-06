// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericsWithoutTypeParameters1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C<T> {
    foo(): T { return null }
    //~^ ERROR: Type 'null' is not assignable to type 'T'.
}

interface I<T> {
    bar(): T;
}

var c1: C;
//~^ ERROR: Generic type 'C<T>' requires 1 type argument.
var i1: I;
//~^ ERROR: Generic type 'I<T>' requires 1 type argument.
var c2: C<I>;
//~^ ERROR: Generic type 'I<T>' requires 1 type argument.
var i2: I<C>;
//~^ ERROR: Generic type 'C<T>' requires 1 type argument.

function foo(x: C, y: I) { }
//~^ ERROR: Generic type 'C<T>' requires 1 type argument.
//~| ERROR: Generic type 'I<T>' requires 1 type argument.
function foo2(x: C<I>, y: I<C>) { }
//~^ ERROR: Generic type 'I<T>' requires 1 type argument.
//~| ERROR: Generic type 'C<T>' requires 1 type argument.

var x: { a: C } = { a: new C<number>() };
//~^ ERROR: Generic type 'C<T>' requires 1 type argument.
var x2: { a: I } = { a: { bar() { return 1 } } };
//~^ ERROR: Generic type 'I<T>' requires 1 type argument.

class D<T> {
    x: C;
//~^ ERROR: Generic type 'C<T>' requires 1 type argument.
    y: D;
//~^ ERROR: Generic type 'D<T>' requires 1 type argument.
}

interface J<T> {
    x: I;
//~^ ERROR: Generic type 'I<T>' requires 1 type argument.
    y: J;
//~^ ERROR: Generic type 'J<T>' requires 1 type argument.
}

class A<T> { }
function f<T>(x: T): A {
//~^ ERROR: Generic type 'A<T>' requires 1 type argument.
    return null;
}