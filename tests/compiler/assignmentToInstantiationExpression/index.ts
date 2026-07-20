// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentToInstantiationExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

let obj: { fn?: <T>() => T } = {};
obj.fn<number> = () => 1234;
//~^ ERROR: The left-hand side of an assignment expression must be a variable or a property access.


let getValue: <T>() => T;
getValue<number> = () => 1234;
//~^ ERROR: The left-hand side of an assignment expression must be a variable or a property access.
//~| ERROR: Variable 'getValue' is used before being assigned.
//~| ERROR: Variable 'getValue' is used before being assigned.

let getValue2!: <T>() => T;
getValue2<number> = () => 1234;
//~^ ERROR: The left-hand side of an assignment expression must be a variable or a property access.
