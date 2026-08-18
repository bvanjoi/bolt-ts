// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTypeOfIndexedAccessParameter.ts`, Apache-2.0 License
//@compiler-options: strict
f('a', {
  cb: (p) => (p)  
});
function g(x, y) {
  x = y;
}