// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/undeclaredBase.ts`, Apache-2.0 License

module M { export class C extends M.I { } }
//~^ ERROR: Property 'I' does not exist on type 'typeof M'.