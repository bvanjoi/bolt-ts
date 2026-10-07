// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualExpressionTypecheckingDoesntBlowStack.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: lib=[es6]
//@compiler-options: strict
// repro for: https://github.com/Microsoft/TypeScript/issues/23661
export default class Operation {
  validateParameters(parameterValues) {
    var result = null;
    for ( var parameterLocation of Object.keys(parameterValues)) {
      var parameter = (this).getParameter();
      ;
      var values = (this).getValues();
      var innerResult = parameter.validate(values[parameter.oaParameter.name]);
      if (innerResult && innerResult.length > 0) {
        // Commenting out this line will fix the problem.
        result = (result || []).concat(innerResult);
      }
      
    }
    return result;
  }
}