// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/dynamicModuleTypecheckError.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: module=commonjs
var x = 1;
for ( var i = 0; i < 30; i++) {
  x = i * 1000;
}