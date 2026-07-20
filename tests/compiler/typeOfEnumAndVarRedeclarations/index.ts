// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeOfEnumAndVarRedeclarations.ts`, Apache-2.0 License

//@compiler-options: target=es2015

enum E {
    a
}
enum E {
    b = 1
}
var x = E;
var x: { readonly a: E; readonly b: E; readonly [x: number]: string; }; // Shouldnt error
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type '{ a: E.a; b: E.b; }', but here has type '{ [x: number]: string }'.
var y = E;
var y: { readonly a: E; readonly b: E; readonly [x: number]: string; readonly [x: number]: string } // two errors: the types are not identical and duplicate signatures
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'y' must be of type '{ a: E.a; b: E.b; }', but here has type '{ [x: number]: string }'.
//~| ERROR: Duplicate index signature for type 'number'.
//~| ERROR: Duplicate index signature for type 'number'.
