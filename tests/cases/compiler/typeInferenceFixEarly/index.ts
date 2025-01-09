//@ run-fail

// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/typeInferenceFixEarly.ts`, Apache-2.0 License

declare function f<T>(p: (t: T) => T): T;

f(n => 3);