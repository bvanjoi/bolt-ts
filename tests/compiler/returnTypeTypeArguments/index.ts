// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/returnTypeTypeArguments.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class One<T>{
    value: T;
    //~^ ERROR: Property 'value' has no initializer and is not definitely assigned in the constructor.
}
class Two<T, U>{
    value: T;
    //~^ ERROR: Property 'value' has no initializer and is not definitely assigned in the constructor.
    id: U;
    //~^ ERROR: Property 'id' has no initializer and is not definitely assigned in the constructor.
}
class Three<T, U, V>{
    value: T;
    //~^ ERROR: Property 'value' has no initializer and is not definitely assigned in the constructor.
    id: U;
    //~^ ERROR: Property 'id' has no initializer and is not definitely assigned in the constructor.
    name: V;
    //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
}

function A1(): One { return null; }
//~^ ERROR: Generic type 'One<T>' requires 1 type argument.
function A2(): Two { return null; }
//~^ ERROR: Generic type 'Two<T, U>' requires 2 type arguments.
function A3(): Three { return null; }
//~^ ERROR: Generic type 'Three<T, U, V>' requires 3 type arguments.

function B1(): Two<number> { return null; }
//~^ ERROR: Generic type 'Two<T, U>' requires 2 type arguments.
function B2(): Three<string> { return null; }
//~^ ERROR: Generic type 'Three<T, U, V>' requires 3 type arguments.
function B3(): Three<string, number> { return null; }
//~^ ERROR: Generic type 'Three<T, U, V>' requires 3 type arguments.

class C {
    A1(): One { return null; }
    //~^ ERROR: Generic type 'One<T>' requires 1 type argument.
    A2(): Two { return null; }
    //~^ ERROR: Generic type 'Two<T, U>' requires 2 type arguments.
    A3(): Three { return null; }
    //~^ ERROR: Generic type 'Three<T, U, V>' requires 3 type arguments.

    B1(): Two<number> { return null; }
    //~^ ERROR: Generic type 'Two<T, U>' requires 2 type arguments.
    B2(): Three<string> { return null; }
    //~^ ERROR: Generic type 'Three<T, U, V>' requires 3 type arguments.
    B3(): Three<string, number> { return null; }
    //~^ ERROR: Generic type 'Three<T, U, V>' requires 3 type arguments.
}

class D<T> {
    A2(): Two<T> { return null; }
    //~^ ERROR: Generic type 'Two<T, U>' requires 2 type arguments.
    A3(): Three<T> { return null; }
    //~^ ERROR: Generic type 'Three<T, U, V>' requires 3 type arguments.

    B1(): Two<T> { return null; }
    //~^ ERROR: Generic type 'Two<T, U>' requires 2 type arguments.
    B2(): Three<T> { return null; }
    //~^ ERROR: Generic type 'Three<T, U, V>' requires 3 type arguments.
    B3(): Three<string, T> { return null; }
    //~^ ERROR: Generic type 'Three<T, U, V>' requires 3 type arguments.
}

interface I<T> {
    value: T;
}

class Y<T>
{
    value: T;
    //~^ ERROR: Property 'value' has no initializer and is not definitely assigned in the constructor.
}

class X<T>
{
    p1: () => X;
    //~^ ERROR: Generic type 'X<T>' requires 1 type argument.
    //~| ERROR: Property 'p1' has no initializer and is not definitely assigned in the constructor.
    p2: { [idx: number]: X }
    //~^ ERROR: Generic type 'X<T>' requires 1 type argument.
    //~| ERROR: Property 'p2' has no initializer and is not definitely assigned in the constructor.
    p3: X[]
    //~^ ERROR: Generic type 'X<T>' requires 1 type argument.
    //~| ERROR: Property 'p3' has no initializer and is not definitely assigned in the constructor.
    p4: I<X>
    //~^ ERROR: Generic type 'X<T>' requires 1 type argument.
    //~| ERROR: Property 'p4' has no initializer and is not definitely assigned in the constructor.
    p5: X
    //~^ ERROR: Generic type 'X<T>' requires 1 type argument.
    p6: () => Y;
    //~^ ERROR: Property 'p6' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: Generic type 'Y<T>' requires 1 type argument.
    p7: { [idx: number]: Y }
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
    //~| ERROR: Property 'p7' has no initializer and is not definitely assigned in the constructor.
    p8: Y[]
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
    //~| ERROR: Property 'p8' has no initializer and is not definitely assigned in the constructor.
    p9: I<Y>
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
    //~| ERROR: Property 'p9' has no initializer and is not definitely assigned in the constructor.
    pa: Y
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
}

declare var a: {
    p1: () => X;
    //~^ ERROR: Generic type 'X<T>' requires 1 type argument.
    p2: { [idx: number]: X }
    //~^ ERROR: Generic type 'X<T>' requires 1 type argument.
    p3: X[]
    //~^ ERROR: Generic type 'X<T>' requires 1 type argument.
    p4: I<X>
    //~^ ERROR: Generic type 'X<T>' requires 1 type argument.
    p5: X
    //~^ ERROR: Generic type 'X<T>' requires 1 type argument.
    p6: () => Y;
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
    p7: { [idx: number]: Y }
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
    p8: Y[]
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
    p9: I<Y>
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
    pa: Y
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
};
