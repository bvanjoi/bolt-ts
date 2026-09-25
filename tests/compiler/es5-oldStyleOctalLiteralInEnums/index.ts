// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es5-oldStyleOctalLiteralInEnums.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

enum E {
  x = -01,
  //~^ ERROR: Octal literals are not allowed.
  y = 02,
  //~^ ERROR: Octal literals are not allowed.
}