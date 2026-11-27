// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inexistentPropertyInsideToStringType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: checkJs
//@compiler-options: noEmit

function toString() {
    this.yadda
    //~^ ERROR: Property 'yadda' does not exist on type 'toString<toString>'.
    this.someValue = "";
}
