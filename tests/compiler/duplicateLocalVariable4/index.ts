// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateLocalVariable4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

enum E{
a
}
 
var x = E;
var x = E.a;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type '{ a: E.a; }', but here has type 'E.a'.