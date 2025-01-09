// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/inferentialTypingWithFunctionType.ts`, Apache-2.0 License

declare function map<T, U>(x: T, f: (s: T) => U): U;
declare function identity<V>(y: V): V;

var s0: string = map("", identity);
var s1: number = map("", identity);
//~^ ERROR: Type 'string' is not assignable to type 'number'.
var s2 = map("", identity);
