// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6ExportAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es6

var a = 10;
export = a;
//~^ ERROR: Export assignment cannot be used when targeting ECMAScript modules. Consider using 'export default' or another module format instead.