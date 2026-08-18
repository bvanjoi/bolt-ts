// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/baseIndexSignatureResolution.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Base { private a: string; }
//~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
class Derived extends Base { private b: string; }
//~^ ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor.

// Note - commmenting "extends Foo" prevents the error
interface Foo {
    [i: number]: Base;
}
interface FooOf<TBase extends Base> extends Foo {
    [i: number]: TBase;
}
var x: FooOf<Derived> = null;
//~^ ERROR: Type 'null' is not assignable to type 'FooOf<Derived>'.
var y: Derived = x[0];

/*
// Note - the equivalent for normal interface methods works fine:
interface A {
    foo(): Base;
}
interface B<TBase extends Base> extends A {
    foo(): TBase;
}
var b: B<Derived> = null;
var z: Derived = b.foo();
*/