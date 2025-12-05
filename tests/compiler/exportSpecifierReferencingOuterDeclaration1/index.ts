// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/exportSpecifierReferencingOuterDeclaration1.ts`, Apache-2.0 License

declare namespace X { export interface bar { } }
declare module "m" {
    export { X };
    //~^ ERROR: Cannot export 'X'. Only local declarations can be exported from a module.
    export function foo(): X.bar;
}