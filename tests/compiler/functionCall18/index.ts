// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionCall18.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare function foo<T>(a: T, b: T);
declare function foo(a: {});
foo<string>("hello");
//~^ ERROR: Expected 2 arguments, but got 1.
