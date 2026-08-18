// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arrowFunctionWithObjectLiteralBody6.ts`, Apache-2.0 License
var a = () => (({
  name: 'foo',
  message: 'bar'  
}));
var b = () => ((({
  name: 'foo',
  message: 'bar'  
})));
var c = () => (({
  name: 'foo',
  message: 'bar'  
}));
var d = () => (((({
  name: 'foo',
  message: 'bar'  
}))));
({
  name: 'foo',
  message: 'bar'  
});
var f = ({
  name: 'foo',
  message: 'bar'  
});