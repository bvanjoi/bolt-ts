// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/functionDeclarationWithResolutionOfTypeOfSameName01.ts`, Apache-2.0 License

interface f {
}

function f() {
    <f>f;
}