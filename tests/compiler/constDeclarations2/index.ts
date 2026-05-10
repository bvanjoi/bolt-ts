// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarations2.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict=false
//@compiler-options: declaration

// No error
namespace M {
    export const c1 = false;
    export const c2: number = 23;
    export const c3 = 0, c4 :string = "", c5 = null;
}