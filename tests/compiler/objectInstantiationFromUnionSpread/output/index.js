// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectInstantiationFromUnionSpread.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictFunctionTypes
function f1(a) {
  a.map((item) => (({
      ...item    
  }))).filter((value) => {});
}
function f2(a) {
  a.map((item) => (({
      ...item    
  }))).filter((value) => {});
}