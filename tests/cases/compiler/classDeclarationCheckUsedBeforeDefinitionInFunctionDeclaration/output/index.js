// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classDeclarationCheckUsedBeforeDefinitionInFunctionDeclaration.ts`, Apache-2.0 License
function f() {
  new C2();
}
// OK
class C2 {}