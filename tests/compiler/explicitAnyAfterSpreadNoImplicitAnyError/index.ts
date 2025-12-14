// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/explicitAnyAfterSpreadNoImplicitAnyError.ts`, Apache-2.0 License

//@compiler-options: noImplicitAny
({ a: [], ...(null as any) });
let x: any;
