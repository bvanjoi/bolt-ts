// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterDestructuringObjectLiteral.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var fn1 = (options) => {};
fn1({
  headers: {
      foo: 1    
  }  
});
var fn2 = ({headers = {}}) => {};
fn2({
  headers: {
      foo: 1    
  }  
});