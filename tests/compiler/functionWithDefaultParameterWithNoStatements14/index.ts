// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/functionWithDefaultParameterWithNoStatements14.ts`, Apache-2.0 License

var v: any[];

function foo(a = v[1 + 1]) { }

function bar(a = v[1 + 1]) {
}