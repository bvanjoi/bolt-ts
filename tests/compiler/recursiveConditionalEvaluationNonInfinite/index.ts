// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveConditionalEvaluationNonInfinite.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

type Test<T> = [T] extends [any[]] ? { array: Test<T[0]> } : { notArray: T };
declare const x: Test<number[]>;
const y: { array: { notArray: number } } = x; // Error
declare const a: Test<number>;
const b: { notArray: number } = a; // Works