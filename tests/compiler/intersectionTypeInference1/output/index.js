function alert(s) {}
var parameterFn = (props) => (alert(props.store));
var brokenFunction = (f) => ((o) => (o));
var Form3 = brokenFunction(parameterFn)({
  store: 'hello'  
});