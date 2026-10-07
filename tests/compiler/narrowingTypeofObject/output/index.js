// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowingTypeofObject.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=true
function test(x) {
  if (typeof x === 'object') {
    x;
  }
  
}
function f1(x) {
  if (typeof x !== 'object') {
    x;
  }
  
}