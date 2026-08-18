// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/intersectionTypeInference1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function alert(s) {}
var parameterFn = (props) => (alert(props.store));
var brokenFunction = (f) => ((o) => (o));
var Form3 = brokenFunction(parameterFn)({
  store: 'hello'  
});