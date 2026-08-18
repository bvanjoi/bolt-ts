// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spreadExpressionContextualType.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
function test(item) {
  return {
      ...item    
  };
}
function test2(item) {
  var x = {
      ...item    
  };
  return x;
}