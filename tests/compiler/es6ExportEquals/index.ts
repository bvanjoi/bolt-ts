// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6ExportEquals.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: declaration

export function f() { }

export = f;
//~^ ERROR: Export assignment cannot be used when targeting ECMAScript modules. Consider using 'export default' or another module format instead.
//~| ERROR: An export assignment cannot be used in a module with other exported elements.
