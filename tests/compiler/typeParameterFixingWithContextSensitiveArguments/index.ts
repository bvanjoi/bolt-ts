// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typeParameterFixingWithContextSensitiveArguments.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

function f<T, U>(y: T, f: (x: T) => U, x: T): [T, U] { return [y, f(x)]; }
interface A { a: A; }
interface B extends A { b; }

var a: A, b: B;

var d = f(b, x => x.a, a); // type [A, A]
var d2 = f(b, x => x.a, null); // type [B, A]
var d3 = f(b, x => x.b, null); // type [B, any]