// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/conflictingTypeAnnotatedVar.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var foo: string;
function foo(): number { }
//~^ ERROR: Duplicate identifier 'foo'.
//~| ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
function foo(): number { }
//~^ ERROR: Duplicate identifier 'foo'.
//~| ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.

