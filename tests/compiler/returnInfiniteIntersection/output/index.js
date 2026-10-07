// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/returnInfiniteIntersection.ts`, Apache-2.0 License
function recursive() {
  var x = (subkey) => (recursive());
  return x;
}
var result = recursive()(1);