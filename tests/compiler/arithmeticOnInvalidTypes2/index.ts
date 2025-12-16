// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arithmeticOnInvalidTypes2.ts`, Apache-2.0 License

var obj = function f<T>(a: T, b: T) {
    var z1 = a + b;
    //~^ ERROR: Operator '+' cannot be applied to types 'T' and 'T'.
    var z2 = a - b;
    //~^ ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    //~| ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    var z3 = a * b;
    //~^ ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    //~| ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    var z4 = a / b;
    //~^ ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    //~| ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    return a;
};