// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reExportUndefined1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export { undefined };
//~^ ERROR: Cannot export 'undefined'. Only local declarations can be exported from a module.