// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/selfReferentialFunctionType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration

declare function f<T>(args: typeof f<T>): T;
declare function g<T = typeof g>(args: T): T;
declare function h<T>(): typeof h<T>;
