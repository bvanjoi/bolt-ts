// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/argumentsObjectIterator01_ES6.ts`, Apache-2.0 License
function doubleAndReturnAsArray(x, y, z) {
  var result = [];
  for ( var arg of arguments) {
    result.push(arg + arg);
  }
  return result;
}