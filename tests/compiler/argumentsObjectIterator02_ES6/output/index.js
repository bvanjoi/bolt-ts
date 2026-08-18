// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/argumentsObjectIterator02_ES6.ts`, Apache-2.0 License
function doubleAndReturnAsArray(x, y, z) {
  var blah = arguments[Symbol.iterator];
  var result = [];
  for ( var arg of blah()) {
    result.push(arg + arg);
  }
  return result;
}