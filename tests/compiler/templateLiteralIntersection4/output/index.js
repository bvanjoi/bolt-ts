// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/templateLiteralIntersection4.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail
var {Provider, useUsername, useAge, useStore} = createStore({
  username: 'Aral',
  age: 31  
});