// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/topLevelLambda3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: module=esnext

export var x = () => this.window;
//~^ ERROR: 'this' is possibly undefined.