// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es5ExportEquals.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration

export function f() { }

export = f;
//~^ ERROR: An export assignment cannot be used in a module with other exported elements.