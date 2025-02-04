// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/objectLiteralWithSemicolons4.ts`, Apache-2.0 License

var v = {
  a   //~ ERROR: Cannot find name 'a'.
;
//~^ ERROR: Expected ','.
//~ ERROR: '}' expected.