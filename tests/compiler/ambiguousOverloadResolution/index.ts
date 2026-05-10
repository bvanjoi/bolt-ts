// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ambiguousOverloadResolution.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

class A { }
class B extends A { x: number; }

declare function f(p: A, q: B): number;
declare function f(p: B, q: A): string;

var x: B;
var t: number = f(x, x); // Not an error