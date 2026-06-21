// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportSpecifierReferencingOuterDeclaration3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare namespace X { export interface bar { } }
declare module "m" {
    namespace X { export interface foo { } }
    export { X };
    export function foo(): X.foo;
    export function bar(): X.bar; // error
    //~^ ERROR: Namespace 'X' has no exported member 'bar'.
}