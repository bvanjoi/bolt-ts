// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typeParameterFixingWithContextSensitiveArguments2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function f<T, U>(y: T, y1: U, p: (z: U) => T, p1: (x: T) => U): [T, U] { return [y, p1(y)]; }
interface A { a: A; }
interface B extends A { b; }

declare var a: A, b: B;

var d = f(a, b, x => x, x => x); // A => A not assignable to A => B
//~^ ERROR: Property 'b' is missing.