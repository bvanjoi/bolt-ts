
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveGenericMethodCall.ts`, Apache-2.0 License
function Generate(func) {
  return Generate(func)
}