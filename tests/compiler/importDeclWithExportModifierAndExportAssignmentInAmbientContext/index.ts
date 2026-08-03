// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importDeclWithExportModifierAndExportAssignmentInAmbientContext.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare module "m" {
    namespace x {
        interface c {
        }
    }
    export import a = x.c;
    export = x;
    //~^ ERROR: An export assignment cannot be used in a module with other exported elements.
}