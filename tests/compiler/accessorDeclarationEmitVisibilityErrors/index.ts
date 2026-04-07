// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/accessorDeclarationEmitVisibilityErrors.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict
//@compiler-options: declaration

export class Q {
    set bet(arg: DoesNotExist) {}
    //~^ ERROR: Cannot find name 'DoesNotExist'.
}