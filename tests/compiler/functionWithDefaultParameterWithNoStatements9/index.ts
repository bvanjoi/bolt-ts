// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionWithDefaultParameterWithNoStatements9.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo(a = console.log) { }

function bar(a = console.log) {
}