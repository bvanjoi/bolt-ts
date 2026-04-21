// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/inferTupleFromBindingPattern.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function f<T>(cb: () => T): T;
const [e1, e2, e3] = f(() => [1, "hi", true]);

const a: 42 = e1;
//~^ ERROR: Type 'number' is not assignable to type '42'.
const b: 42 = e2;
//~^ ERROR: Type 'string' is not assignable to type '42'.
const c: 42 = e3;
//~^ ERROR: Type 'boolean' is not assignable to type '42'.
const d: number = e1;