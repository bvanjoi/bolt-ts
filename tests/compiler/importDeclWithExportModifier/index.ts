// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importDeclWithExportModifier.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=amd

namespace x {
    interface c {
    }
}
export import c = x;
export import a = x.c;
//~^ ERROR: Namespace 'x' has no exported member 'c'.
var b: a;
