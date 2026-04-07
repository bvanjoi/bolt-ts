// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/inferentialTypingWithFunctionTypeNested.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail
declare function map<T, U>(x: T, f: () => { x: (s: T) => U }): U;
declare function identity<V>(y: V): V;

var s = map("", () => { return { x: identity }; });