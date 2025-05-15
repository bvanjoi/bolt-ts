// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/innerOverloads.ts`, Apache-2.0 License
function outer() {
  
  // should work
  
  function inner(a) {
    return a
  }
  return inner(0)
}
var x = outer();