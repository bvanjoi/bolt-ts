// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constDeclarationShadowedByVarDeclaration2.ts`, Apache-2.0 License

//@ compiler-options: target=ES6

function outer() {
    const x = 0;
    function inner() {
        var x = "inner";
    }
}
