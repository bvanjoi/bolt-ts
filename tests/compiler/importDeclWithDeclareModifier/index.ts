// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importDeclWithDeclareModifier.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace x {
    interface c {
    }
}
declare export import a = x.c;
//~^ ERROR: Namespace 'x' has no exported member 'c'.
//~| ERROR: A 'declare' modifier cannot be used with an import declaration.
//~| ERROR: 'export' modifier must precede 'declare' modifier.
var b: a;