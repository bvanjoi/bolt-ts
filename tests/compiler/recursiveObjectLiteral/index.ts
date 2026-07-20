// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveReturns.ts`, Apache-2.0 License

var a = { f: a };
//~^ ERROR: 'a' implicitly has type 'any' because it does not have a type annotation and is referenced directly or indirectly in its own initializer.
