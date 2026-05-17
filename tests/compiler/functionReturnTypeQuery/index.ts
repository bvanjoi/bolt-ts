// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionReturnTypeQuery.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare let foo: number;

declare function test1(foo: string, bar: typeof foo): typeof foo;
declare function test2({foo}: {foo: string}, bar: typeof foo): typeof foo;