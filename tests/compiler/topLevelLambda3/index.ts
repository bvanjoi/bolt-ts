// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/topLevelLambda3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var f = () => {this.window;}
//~^ ERROR: The containing arrow function captures the global value of 'this'.