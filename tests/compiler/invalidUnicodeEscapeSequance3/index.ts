// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/invalidUnicodeEscapeSequance3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

a\u
//~^ ERROR: Declaration or statement expected
//~| ERROR: Cannot find name 'u'.
//~| ERROR: Invalid character.

