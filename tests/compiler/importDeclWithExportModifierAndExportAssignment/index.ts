// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importDeclWithExportModifierAndExportAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

namespace x {
    interface c {
    }
}
export import a = x.c;
//~^ ERROR: Namespace 'x' has no exported member 'c'.
export = x;
//~^ ERROR: An export assignment cannot be used in a module with other exported elements.

export import b = x;
import c = x;