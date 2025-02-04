// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveTypeParameterReferenceError1.ts`, Apache-2.0 License

class X<T> { }
interface Foo<T> {
    z: Foo<X<T>>; // error
}
var f: Foo<number>;
var r = f.z; 
var r0: Foo<X<number>> = f.z;
var r01: Foo<X<string>> = f.z;

class C2<T> {
    x: T;
}
interface Foo2<T> {
    ofC4: C2<{ x: T }> // ok
}
var f2: Foo2<number>;
var r2 = f2.ofC4;
var r20: C2<{ x: number }> = f2.ofC4;
var r21: C2<{ x: string }> = f2.ofC4;
//~^ ERROR: Type 'C2<{ x: number; }>' is not assignable to type 'C2<{ x: string; }>'.
