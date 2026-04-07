// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericCallWithNonGenericArgs1.ts`, Apache-2.0 License

function f<T>(x: any) { }
f<any>(null)
