// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/constWithNonNull.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare const x: number | undefined;
x!++;
//~^ ERROR: Cannot assign to 'x' because it is a constant.