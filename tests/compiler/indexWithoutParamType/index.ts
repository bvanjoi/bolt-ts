// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/indexWithoutParamType.ts`, Apache-2.0 License

var y: { []; } // Error
//~^ ERROR: An index signature must have exactly one parameter.
//~| ERROR: An index signature must have a type annotation.