// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarationShadowedByVarDeclaration2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// No errors, const declaration is not shadowed
function outer() {
    const x = 0;
    function inner() {
        var x = "inner";
    }
}