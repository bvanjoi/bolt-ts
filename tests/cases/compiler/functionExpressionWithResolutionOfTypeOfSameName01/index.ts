// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionExpressionWithResolutionOfTypeOfSameName01.ts`, Apache-2.0 License

interface f {
}

var x = function f() {
    <f>f;
}