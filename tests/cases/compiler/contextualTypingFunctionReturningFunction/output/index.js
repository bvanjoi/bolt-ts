
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTypingFunctionReturningFunction.ts`, Apache-2.0 License
//@ run-fail

f({a: (s) => {},
b: () => (n) => {}});