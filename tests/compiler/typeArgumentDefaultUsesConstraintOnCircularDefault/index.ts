// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeArgumentDefaultUsesConstraintOnCircularDefault.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type Test<T extends string = T> = { value: T };  // Error
//~^ ERROR: Type parameter defaults can only reference previously declared type parameters.

let zz: Test = { foo: "abc" };  // should error on comparison with Test<string>
//~^ ERROR: Object literal may only specify known properties, and 'foo' does not exist in type 'Test<error>'.

let zzy: Test = { value: {} };

// Simplified repro from #28873

class C1<T extends C1 = any> {}

class C2<T extends C2<any> = any> {}
