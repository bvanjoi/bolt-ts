// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLitIndexerContextualType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface I {
    [s: string]: (s: string) => number;
}

interface J {
    [s: number]: (s: string) => number;
}

var x: I;
var y: J;
x = {
    s: t => t * t, // Should error
    //~^ ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    //~| ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    //~| ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    //~| ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
};
x = {
    0: t => t * t, // Should error
    //~^ ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    //~| ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    //~| ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    //~| ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
};
y = {
    s: t => t * t, // Should error
    //~^ ERROR: Object literal may only specify known properties, and 's' does not exist in type 'J'.
};
y = {
    0: t => t * t, // Should error
    //~^ ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    //~| ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    //~| ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
    //~| ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
};
