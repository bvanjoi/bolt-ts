// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionWithDefaultParameterWithNoStatements12.ts`, Apache-2.0 License

var v: any[];

function foo(a = (v)) { }

function bar(a = (v)) {
}