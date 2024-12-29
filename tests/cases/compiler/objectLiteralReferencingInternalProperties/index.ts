// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/objectLiteralReferencingInternalProperties.ts`, Apache-2.0 License

var a = { b: 10, c: b }; 
//~^ ERROR: Cannot find name 'b'.