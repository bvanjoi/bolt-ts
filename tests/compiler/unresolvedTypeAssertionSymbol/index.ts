// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/unresolvedTypeAssertionSymbol.ts`, Apache-2.0 License

var x = 1;
var y = <asdf>x;
//~^ ERROR: Cannot find name 'asdf'.
