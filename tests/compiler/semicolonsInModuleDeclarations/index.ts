// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/semicolonsInModuleDeclarations.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

declare namespace ambiModule {
   export interface i1 { }; //~ERROR: Statements are not allowed in ambient contexts.
   export interface i2 { }
}

var n1: ambiModule.i1;
var n2: ambiModule.i2;
