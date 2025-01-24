// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericFunctionSpecializations1.ts`, Apache-2.0 License

function foo3<T>(test: string); // error
function foo3<T>(test: T) { }

foo3("");

foo3(1);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
foo3<number>(1)
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.

function foo4<T>(test: string); // valid
function foo4<T extends String>(test: T) { }

foo4("");