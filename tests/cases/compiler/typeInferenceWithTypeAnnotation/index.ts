//@ run-fail

// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/typeInferenceWithTypeAnnotation.ts`, Apache-2.0 License

declare function f<T>(p: (t: T) => T): T;

f((n: number) => n); 

let n0: number = f((n: number) => n);
let n1: string = f((n: string) => n);