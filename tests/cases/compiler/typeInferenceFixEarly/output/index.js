//@ run-fail
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeInferenceFixEarly.ts`, Apache-2.0 License

f((n) => 3);
var a = f((n) => 42);
var b = f((n) => "");