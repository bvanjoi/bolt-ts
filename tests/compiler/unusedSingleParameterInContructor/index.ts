// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedSingleParameterInContructor.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class Dummy {
    constructor(person: string) {
        //~^ ERROR: 'person' is declared but its value is never read.
        var unused = 20;
        //~^ ERROR: 'unused' is declared but its value is never read.
    }
}