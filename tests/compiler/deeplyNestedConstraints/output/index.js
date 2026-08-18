// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/deeplyNestedConstraints.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
class BufferPool {
  setArray2(_, array) {
    array.length;
  }
}