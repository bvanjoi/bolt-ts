// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/constraintErrors1.ts`, Apache-2.0 License

function foo5<T extends hm>(test: T) { }
//~^ ERROR: Cannot find name 'hm'.