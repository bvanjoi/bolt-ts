// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectFreeze.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const f = Object.freeze(function foo(a: number, b: string) { return false; });
f(1, "") === false;

class C { constructor(a: number) { } }
const c = Object.freeze(C);
new c(1);

const a = Object.freeze([1, 2, 3]);
a[0] = a[2].toString();
//~^ ERROR: Type 'string' is not assignable to type 'number'.
//~| ERROR: Index signature in type 'number[]' only permits reading.

const o = Object.freeze({ a: 1, b: "string", c: true });
o.b = o.a.toString();
//~^ ERROR: Cannot assign to 'b' because it is a read-only property.
