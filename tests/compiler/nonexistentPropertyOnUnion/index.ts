// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonexistentPropertyOnUnion.ts`, Apache-2.0 License

//@compiler-options: target=es2015
function f(x: string | Promise<string>) {
    x.toLowerCase();
    //~^ ERROR: Property 'toLowerCase' does not exist on type 'string | Promise<string>'.
}
