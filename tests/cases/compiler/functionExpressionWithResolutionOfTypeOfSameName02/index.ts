// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionExpressionWithResolutionOfTypeOfSameName02.ts`, Apache-2.0 License

interface Foo {
}

var x = function Foo() {
    var x: Foo;
}