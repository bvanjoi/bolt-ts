// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericMergedDeclarationUsingTypeParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo<T extends U, U>(y: T, z: U) { return y; }
namespace foo {
    export var x: T;
    //~^ ERROR: Cannot find name 'T'.
    var y = <T>1;
    //~^ ERROR: Cannot find name 'T'.
}