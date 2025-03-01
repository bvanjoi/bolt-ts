// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/forInStatement2.ts`, Apache-2.0 License

var expr: number;
for (var a in expr) {
//~^ ERROR: The right-hand side of a 'for...in' statement must be of type 'any', an object type or a type parameter, but here has type 'number'.
}