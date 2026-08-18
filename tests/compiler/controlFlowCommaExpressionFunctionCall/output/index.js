// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowCommaExpressionFunctionCall.ts`, Apache-2.0 License
var otherValue = () => (true);
var value = null;
function isNumber(obj) {
  return true;
}
if (isNumber((otherValue() , value))) {
  var b = value;
}
