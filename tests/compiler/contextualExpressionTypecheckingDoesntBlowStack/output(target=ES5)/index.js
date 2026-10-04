export default class Operation {
  validateParameters(parameterValues) {
    var result = null;
    for ( var parameterLocation of Object.keys(parameterValues)) {
      var parameter = (this).getParameter();
      ;
      var values = (this).getValues();
      var innerResult = parameter.validate(values[parameter.oaParameter.name]);
      if (innerResult && innerResult.length > 0) {
        result = (result || []).concat(innerResult);
      }
      
    }
    return result;
  }
}