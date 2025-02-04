// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionCall18.ts`, Apache-2.0 License

declare function foo<T>(a: T, b: T);
declare function foo(a: {});
foo<string>("hello");
//~^ ERROR: Expected 2 arguments, but got 1.