// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/forInStatement7.ts`, Apache-2.0 License

var a: number;
var expr: any;
for (a in expr) {
//~^ ERROR: The left-hand side of a 'for...in' statement must be of type 'string' or 'any'.
}