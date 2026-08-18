// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/invalidUnicodeEscapeSequance4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var a\u0031; // a1 is a valid identifier
var \u0031a; // 1a is an invalid identifier
//~^ ERROR: Invalid character.