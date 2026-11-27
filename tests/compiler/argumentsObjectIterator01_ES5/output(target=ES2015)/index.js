// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/argumentsObjectIterator01_ES5.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
function doubleAndReturnAsArray(x, y, z) {
  var result = [];
  for ( var arg of arguments) {
    //~[target=ES5]^      ERROR: Type 'IArguments' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
    result.push(arg + arg);
  }
  return result;
}