// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/booleanAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var b = new Boolean();
b = 1; // Error
//~^ ERROR: Type 'number' is not assignable to type 'Boolean'.
b = "a"; // Error
//~^ ERROR: Type 'string' is not assignable to type 'Boolean'.
b = {}; // Error
//~^ ERROR: Type '{ }' is not assignable to type 'Boolean'.

var o = {};
o = b; // OK

b = true; // OK

declare var b2:boolean;
b = b2; // OK