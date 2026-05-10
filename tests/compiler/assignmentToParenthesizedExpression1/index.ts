// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentToObjectAndFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x;
(1, x)=0;
//~^ ERROR: The left-hand side of an assignment expression must be a variable or a property access.