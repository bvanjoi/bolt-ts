// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/objectLiteralWithSemicolons3.ts`, Apache-2.0 License

var v = {
  a;
  b;
  c;
}
//~^^^^ ERROR: Expected ','.
//~| ERROR: Cannot find name 'a'.
//~^^^^^ ERROR: Expected ','.
//~| ERROR: Cannot find name 'b'.
//~^^^^^^ ERROR: Expected ','.
//~| ERROR: Cannot find name 'c'.
