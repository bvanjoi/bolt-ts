// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/matchingOfObjectLiteralConstraints.ts`, Apache-2.0 License

function foo2<T, U extends { y: T; }>(x: U, z: T) { }
foo2({ y: "foo" }, "foo");
foo2({ y: 42 }, "foo");
//~^ ERROR: Type 'number' is not assignable to type 'string'. 
