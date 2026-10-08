// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/templateExpressionNoInlininingOfConstantBindingWithInitializer.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function example(parameters) {
  var {value = '123'} = parameters;
  return `${value}` === '345';
}
function example2(parameters) {
  var {value = '123'} = parameters;
  var b = `${value}`;
  return b;
}