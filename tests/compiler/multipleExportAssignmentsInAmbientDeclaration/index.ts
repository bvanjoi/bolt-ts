// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/multipleExportAssignmentsInAmbientDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare module "m1" {
    var a: number
    var b: number;
    export = a;
    export = b;
    //~^ ERROR: Duplicate identifier 'export ='.
}