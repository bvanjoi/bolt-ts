// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/mappedTypeNestedGenericInstantiation.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var square = (x) => (x * x);
var v = chain({
  a: 1,
  b: 2  
}).mapValues(square).value();