// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/generics4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C<T> { private x: T; }
//~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
interface X { f(): string; }
interface Y { f(): boolean; }
declare var a: C<X>;
declare var b: C<Y>;

a = b; // Not ok - return types of "f" are different
//~^ ERROR: Type 'C<Y>' is not assignable to type 'C<X>'.