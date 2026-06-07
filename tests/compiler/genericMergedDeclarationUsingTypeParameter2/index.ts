// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericMergedDeclarationUsingTypeParameter2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class foo<T> { constructor(x: T) { } }
namespace foo {
    export var x: T;
    //~^ ERROR: Cannot find name 'T'.
    var y = <T>1;
    //~^ ERROR: Cannot find name 'T'.
}