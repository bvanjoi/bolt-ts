// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportSpecifierAndLocalMemberDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare module "m2" {
    namespace X {
        interface I { }
    }
    function Y();
    export { Y as X };
    function Z(): X.I;
}

declare module "m2" {
    function Z2(): X.I;
    //~^ ERROR: Cannot find name 'X'.
}