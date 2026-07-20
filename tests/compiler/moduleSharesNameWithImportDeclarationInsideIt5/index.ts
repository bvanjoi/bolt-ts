// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleSharesNameWithImportDeclarationInsideIt5.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Z {
    export namespace M {
        export function bar() {
            return "";
        }
    }
    export interface I { }
}
namespace A.M {
    import M = Z.I;
    import M = Z.M;
    //~^ ERROR: Duplicate identifier 'M'.

    export function bar() {
    }
    M.bar(); // Should call Z.M.bar
    //~^ ERROR: Cannot find name 'M'.
}
