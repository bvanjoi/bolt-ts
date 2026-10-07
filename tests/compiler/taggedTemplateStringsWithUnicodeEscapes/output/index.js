// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedTemplateStringsWithUnicodeEscapes.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function f(...args) {
  console.log(args);
}
f`'💩'${' should be converted to '}'\uD83D\uDCA9'`;