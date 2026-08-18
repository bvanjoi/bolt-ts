// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportDeclarationsInAmbientNamespaces2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare module "mod" {
    export var x: number;
}

declare namespace N {
    export { x } from "mod"; // Error
    //~^ ERROR: Export declarations are not permitted in a namespace.
}
