// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseTypeInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
var $$x = load('something').then((s) => (convert(s)));