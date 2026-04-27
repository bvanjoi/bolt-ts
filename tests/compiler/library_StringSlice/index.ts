// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/library_StringSlice.ts`, Apache-2.0 License

//@compiler-options: target=es2015
// String.prototype.slice can have zero, one, or two arguments
String.prototype.slice();
String.prototype.slice(0);
String.prototype.slice(0,1);

const a: number = String.prototype.slice(0,1);
//~^ ERROR: Type 'string' is not assignable to type 'number'.
