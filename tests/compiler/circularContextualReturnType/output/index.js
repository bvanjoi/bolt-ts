// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/circularContextualReturnType.ts`, Apache-2.0 License
Object.freeze({
  foo() {
    return Object.freeze('a');
  }  
});