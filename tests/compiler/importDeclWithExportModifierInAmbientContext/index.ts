// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importDeclWithExportModifierInAmbientContext.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare module "m" {
    namespace x {
        interface c {
        }
    }
    export import a = x.c;
    var b: a;
}
