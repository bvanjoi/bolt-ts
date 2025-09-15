// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/functionExpressionWithResolutionOfTypeNamedArguments01.ts`, Apache-2.0 License

interface arguments {
}

var x = function f() {
    <arguments>arguments;
}