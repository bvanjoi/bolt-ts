// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleSharesNameWithImportDeclarationInsideIt6.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Z.M {
    export function bar() {
        return "";
    }
}
namespace A.M {
    import M = Z.M;
    export function bar() {
    }
}