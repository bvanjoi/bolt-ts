// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalParameterRetainsNull.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

interface Bar {  bar: number; foo: object | null;  }

let a = {
  test<K extends keyof Bar> (a: K,  b?: Bar[K]  |  null)  { }
};
a.test("bar", null); // ok, null is assignable to number | null | undefined

type B = keyof Bar;
let b0: B = "foo";
let b1: B = "bar";
let b2: B = "42";
//~^ ERROR: Type 'string' is not assignable to type '"bar" | "foo"'.

interface Foo {  bar: number  }

let a0 = {
  test<K extends keyof Foo> (a: K)  { }
};
a0.test("bar");
