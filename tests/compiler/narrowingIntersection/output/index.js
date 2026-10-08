// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowingIntersection.ts`, Apache-2.0 License
//@compiler-options: target=es2015
// Somehow this being an intersection matters.
function test1(result) {
  if (result.err) {
    throw result.err
  }
  
  return // Error, should OK
  result.value;
}
function want0(x) {}
function test2(a) {
  if (a === 0) {
    want0(a);// Fails, but expect to work
    
  }
  
}