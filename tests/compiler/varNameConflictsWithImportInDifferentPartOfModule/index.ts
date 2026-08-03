// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/varNameConflictsWithImportInDifferentPartOfModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M1 {
    export var q = 5;
    export var s = '';
}
namespace M1 {
    export import q = M1.s; // Should be an error but isn't
    //~^ ERROR: Import declaration conflicts with local declaration of 'q'.
}