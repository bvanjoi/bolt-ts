// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/forInStatement4.ts`, Apache-2.0 License

var expr: any;
for (var a: number in expr) {
  //~^ ERROR: The left-hand side of a 'for...in' statement cannot use a type annotation.
}

for (var b: number of expr) {
  //~^ ERROR: The left-hand side of a 'for...of' statement cannot use a type annotation.
}