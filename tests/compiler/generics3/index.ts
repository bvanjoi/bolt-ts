// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/generics3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

class C<T> { private x: T; }
//~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
interface X { f(): string; }
interface Y { f(): string; }
var a: C<X>;
var b: C<Y>;

a = b; // Ok - should be identical
//~^ ERROR: Variable 'b' is used before being assigned.