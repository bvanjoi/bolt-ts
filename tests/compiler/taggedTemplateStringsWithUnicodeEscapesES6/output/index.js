// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedTemplateStringsWithUnicodeEscapesES6.ts`, Apache-2.0 License
//@compiler-options: target=es6
function f(...args) {}
f`'💩'${' should be converted to '}'\uD83D\uDCA9'`;