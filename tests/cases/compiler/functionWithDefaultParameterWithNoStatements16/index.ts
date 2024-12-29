// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/functionWithDefaultParameterWithNoStatements16.ts`, Apache-2.0 License

var v: any[];

function foo(a = bar()) { }

function bar(a = foo()) {
}