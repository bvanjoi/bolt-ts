// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportDefaultAsyncFunction.ts`, Apache-2.0 License

//@compiler-options: target=es6
export default async function foo(): Promise<void> {}
foo();
