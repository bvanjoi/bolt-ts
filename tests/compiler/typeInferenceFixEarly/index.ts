//@ run-fail

// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeInferenceFixEarly.ts`, Apache-2.0 License

declare function f<T>(p: (t: T) => T): T;

f(n => 3);

let a: number = f(n => 42);
let b: string = f(n => '');