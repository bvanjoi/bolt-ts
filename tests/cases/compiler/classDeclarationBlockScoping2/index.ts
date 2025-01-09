// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/classDeclarationBlockScoping2.ts`, Apache-2.0 License

function f() {
    class C {}
    var c1 = C;
    {
        class C {}
        var c2 = C;
    }
    return C === c1;
}