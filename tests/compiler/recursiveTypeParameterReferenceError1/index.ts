// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveTypeParameterReferenceError1.ts`, Apache-2.0 License

class X<T> { }
interface Foo<T> {
    z: Foo<X<T>>; // error
}
var f: Foo<number>;
var r = f.z; 
//~^ ERROR: Variable 'f' is used before being assigned.
var r0: Foo<X<number>> = f.z;
//~^ ERROR: Variable 'f' is used before being assigned.
var r01: Foo<X<string>> = f.z;
//~^ ERROR: Variable 'f' is used before being assigned.

class C2<T> {
    x: T;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}
interface Foo2<T> {
    ofC4: C2<{ x: T }> // ok
}
var f2: Foo2<number>;
var r2 = f2.ofC4;
//~^ ERROR: Variable 'f2' is used before being assigned.
var r20: C2<{ x: number }> = f2.ofC4;
//~^ ERROR: Variable 'f2' is used before being assigned.
var r21: C2<{ x: string }> = f2.ofC4;
//~^ ERROR: Type 'C2<{ x: number; }>' is not assignable to type 'C2<{ x: string; }>'.
//~| ERROR: Variable 'f2' is used before being assigned.
