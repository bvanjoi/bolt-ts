// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/localTypeParameterInferencePriority.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function f() {
  try {} catch (e) {
    var stack2 = e.stack;
    return stack2;
  }
}