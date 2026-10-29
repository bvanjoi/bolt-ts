// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unicodeIdentifierName2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var a₁ = "hello"; alert(a₁)
//~^ ERROR: Invalid character.
//~| ERROR: Expected ','.
//~| ERROR: Invalid character.
//~| ERROR: Expected ','.
//~| ERROR: Argument expression expected.

