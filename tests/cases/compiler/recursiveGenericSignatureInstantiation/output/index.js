// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveGenericTypeHierarchy.ts`, Apache-2.0 License
function f6(x) {
  return f6(x)
}