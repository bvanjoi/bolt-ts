// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/functionWithDefaultParameterWithNoStatements11.ts`, Apache-2.0 License

var v: any[];

function foo(a = v[0]) { }

function bar(a = v[0]) {
}