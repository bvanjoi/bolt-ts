// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/invalidUnicodeEscapeSequance2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var arg\uxxxx
//~^ ERROR: Invalid character.
