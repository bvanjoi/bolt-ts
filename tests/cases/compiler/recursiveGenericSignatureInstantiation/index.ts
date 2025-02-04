// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveGenericTypeHierarchy.ts`, Apache-2.0 License

function f6<T>(x: T) {
  return f6(x);
}
