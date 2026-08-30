// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importDeclWithClassModifiers.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

namespace x {
    interface c {
    }
}
export public import a = x.c;
//~^ ERROR: 'public' modifier cannot appear on a module or namespace element.
//~| ERROR: Namespace 'x' has no exported member 'c'.
export private import b = x.c;
//~^ ERROR: 'private' modifier cannot appear on a module or namespace element.
//~| ERROR: Namespace 'x' has no exported member 'c'.
export static import c = x.c;
//~^ ERROR: Namespace 'x' has no exported member 'c'.
//~| ERROR: Declaration or statement expected.
//~| ERROR: Declaration or statement expected.
var b: a;
