// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/incompatibleExports1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare module "foo" { 
    export interface x { a: string } 
    interface y { a: Date }
    export = y;
    //~^ ERROR: An export assignment cannot be used in a module with other exported elements.
}
 
declare module "baz" {
    export namespace a {
        export var b: number;
    }
 
    namespace c {
        export var c: string;
    }
 
    export = c;
    //~^ ERROR: An export assignment cannot be used in a module with other exported elements.
}
