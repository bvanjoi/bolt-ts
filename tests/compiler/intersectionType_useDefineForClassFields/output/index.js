// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/intersectionType_useDefineForClassFields.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function bar(_p) {
  return null;
}
class Baz extends bar({
  x: 1  
}) {}