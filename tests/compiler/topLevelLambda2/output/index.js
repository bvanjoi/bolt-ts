// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/topLevelLambda2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo(x) {}
foo(() => (this.window));