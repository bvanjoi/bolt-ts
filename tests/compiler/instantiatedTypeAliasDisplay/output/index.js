// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/instantiatedTypeAliasDisplay.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
//@run-fail
var x1 = f1();
var x2 = // Z<string, number>
f2({}, {}, {}, {});