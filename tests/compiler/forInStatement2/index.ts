// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forInStatement2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var expr: number;
for (var a in expr) {
//~^ ERROR: The right-hand side of a 'for...in' statement must be of type 'any', an object type or a type parameter, but here has type 'number'.
}
