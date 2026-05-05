// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionDeclarationWithResolutionOfTypeNamedArguments01.ts`, Apache-2.0 License

//@compiler-options: target=es2015
interface arguments {
}

function f() {
    <arguments>arguments;
}