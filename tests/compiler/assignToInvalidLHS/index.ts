// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignToInvalidLHS.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var y:any;

// Below is actually valid JavaScript (see http://es5.github.com/#x8.7 ), even though will always fail at runtime with 'invalid left-hand side'
var x = new y = 5;
//~^ ERROR: The left-hand side of an assignment expression must be a variable or a property access.