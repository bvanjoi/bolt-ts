// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/selfReferencesInFunctionParameters.ts`, Apache-2.0 License

function foo(x: number = x) {
  //~^ ERROR: Parameter 'x' cannot reference itself.
}

function bar(x0 = "", x: number = x) {
  //~^ ERROR: Parameter 'x' cannot reference itself.
}

class C {
    constructor(x = 1, y = y) {
        //~^ ERROR: Parameter 'y' cannot reference itself.
    }
     
    bar(a = "", b: string = b.toString()) {
        //~^ ERROR: Parameter 'b' cannot reference itself.
    }
}