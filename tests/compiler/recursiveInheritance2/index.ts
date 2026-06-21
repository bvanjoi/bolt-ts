// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveInheritance2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

interface A { (): B; };
declare var a: A;
var x = a();

interface B { (): C; };
declare var b: B;
var y = b();

interface C { (): A; };
declare var c: C;
var z = c();

x = y;
