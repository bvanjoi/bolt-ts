// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/globalThisCapture.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
(() => (this.window));
var parts = [];
parts[0];