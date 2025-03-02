// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/arrowFunctionInConstructorArgument1.ts`, Apache-2.0 License

class C {
    constructor(x: () => void) { }
}
var c = new C(() => { return asdf; } )
//~^ ERROR: Cannot find name 'asdf'.
