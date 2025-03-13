// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeofInObjectLiteralType.ts`, Apache-2.0 License

var a: { b: number; c: typeof b };
//~^ ERROR: Cannot find name 'b'.