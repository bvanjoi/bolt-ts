// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericTypeAssertions3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var r = < <T>(x: T) => T > ((x) => { return null; }); // bug was 'could not find dotted symbol T' on x's annotation in the type assertion instead of no error
var s = < <T>(x: T) => T > ((x: any) => { return null; }); // no error
