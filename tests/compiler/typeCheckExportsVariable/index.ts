// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeCheckExportsVariable.ts`, Apache-2.0 License

let exports: number;
exports = '';
//~^ ERROR: Type 'string' is not assignable to type 'number'.