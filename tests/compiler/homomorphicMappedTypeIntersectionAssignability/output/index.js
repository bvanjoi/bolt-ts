// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/homomorphicMappedTypeIntersectionAssignability.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function f(a, b, c) {
  c = a;
  b = a;
}