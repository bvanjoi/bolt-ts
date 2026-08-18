// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/incompatibleExports2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare module "foo" { 
    export interface x { a: string } 
    interface y { a: Date }
    export = y;
    //~^ ERROR: An export assignment cannot be used in a module with other exported elements.
}
