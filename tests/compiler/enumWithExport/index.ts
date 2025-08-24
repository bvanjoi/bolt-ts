// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumWithExport.ts`, Apache-2.0 License

namespace x {
  export let y = 123
}
enum x {
  z = y
  //~^ ERROR: Cannot find name 'y'.
}