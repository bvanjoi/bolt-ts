// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsinConstructor2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class greeter {
    constructor() {
        var unused = 20;
        //~^ ERROR: 'unused' is declared but its value is never read.
        var used = "dummy";
        used = used + "second part";
    }
}