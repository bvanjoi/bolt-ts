// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/restParameterNotLast.ts`, Apache-2.0 License

function f(...x, y) { }
//~^ ERROR: A rest parameter must be last in a parameter list.