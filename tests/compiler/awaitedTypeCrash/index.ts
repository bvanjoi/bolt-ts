// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/awaitedTypeCrash.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict=false

// https://github.com/microsoft/TypeScript/issues/51984
async function* f<T extends Promise<never>>(): AsyncGenerator<T, void, void> { }