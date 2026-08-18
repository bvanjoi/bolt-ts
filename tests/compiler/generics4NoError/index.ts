// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/generics4NoError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

class C<T> { private x: T; }
//~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
interface X { f(): string; }
interface Y { f(): boolean; }
var a: C<X>;
var b: C<Y>;
