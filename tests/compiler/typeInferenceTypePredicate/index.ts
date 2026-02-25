// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeInferenceTypePredicate.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function f<T>(predicate: (x: {}) => x is T): T;
// 'res' should be of type 'number'.
const res = f((n): n is number => true);
const res2: string = f((n): n is number => true);
//~^ ERROR: Type 'number' is not assignable to type 'string'.
