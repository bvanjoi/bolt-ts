// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/ExportAssignment7.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export class C {
}

export = B;
//~^ ERROR: An export assignment cannot be used in a module with other exported elements.
//~| ERROR: Cannot find name 'B'.