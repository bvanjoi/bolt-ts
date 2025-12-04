// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/errorOnContextuallyTypedReturnType.ts`, Apache-2.0 License

var n1: () => boolean = function () { }; // expect an error here
//~^ ERROR: Type '() => void' is not assignable to type '() => boolean'.
var n2: () => boolean = function ():boolean { }; // expect an error here
//~^ ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
