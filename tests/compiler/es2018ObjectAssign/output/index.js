// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es2018ObjectAssign.ts`, Apache-2.0 License
//@compiler-options: target=es2018
//@run-fail
var test = Object.assign({}, {
  test: true  
});

p.finally();