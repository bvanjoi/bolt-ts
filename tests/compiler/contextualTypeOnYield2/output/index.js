// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/contextualTypeOnYield2.ts`, Apache-2.0 License
//@compiler-options: target=es6
var g = function* () {
  return (num) => (console.log(num));
};