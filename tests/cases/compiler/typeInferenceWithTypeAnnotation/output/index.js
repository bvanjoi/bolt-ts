//@ run-fail
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeInferenceWithTypeAnnotation.ts`, Apache-2.0 License

f((n) => n);
var n0 = f((n) => n);
var n1 = f((n) => n);