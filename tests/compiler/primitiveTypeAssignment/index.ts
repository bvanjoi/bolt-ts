// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/primitiveTypeAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x = any;
//~^ ERROR: Cannot find name 'any'.

var y = number;
//~^ ERROR: Cannot find name 'number'.

var z = boolean;
//~^ ERROR: Cannot find name 'boolean'.
