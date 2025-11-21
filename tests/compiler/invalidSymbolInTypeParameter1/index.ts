// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/invalidSymbolInTypeParameter1.ts`, Apache-2.0 License

function test() {
    var cats = new Array<WAWA>(); // WAWA is not a valid type
    //~^ ERROR: Cannot find name 'WAWA'.
}
