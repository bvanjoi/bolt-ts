// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arithmeticOnInvalidTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x: Number;
var y: Number;
var z = x + y;
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'y' is used before being assigned.
//~| ERROR: Operator '+' cannot be applied to types 'Number' and 'Number'.
var z2 = x - y;
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'y' is used before being assigned.
//~| ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
//~| ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
var z3 = x * y;
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'y' is used before being assigned.
//~| ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
//~| ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
var z4 = x / y;
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'y' is used before being assigned.
//~| ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
//~| ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
