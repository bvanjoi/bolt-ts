// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/redeclarationOfVarWithGenericType.ts`, Apache-2.0 License

var a1: { fn<T>(x: T): T };
var a1: { fn<T>(x: T): T };