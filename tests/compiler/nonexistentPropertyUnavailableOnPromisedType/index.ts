// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nonexistentPropertyUnavailableOnPromisedType.ts`, Apache-2.0 License

function f(x: Promise<number>) {
  x.toLowerCase();
  //~^ ERROR: Property 'toLowerCase' does not exist on type 'Promise<number>'.
}
