// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/argumentsObjectIterator03_ES6.ts`, Apache-2.0 License
function asReversedTuple(a, b, c) {
  var [x, y, z] = arguments;
  return [z, y, x];
}