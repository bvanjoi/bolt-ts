// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/arrowFunctionMissingCurlyWithSemicolon.ts`, Apache-2.0 License

// Should error at semicolon.
var f = () => ;
//~^ ERROR: Expression expected.
var b = 1 * 2 * 3 * 4;
var square = (x: number) => x * x;