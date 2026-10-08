// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedParametersinConstructor2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class greeter {
    constructor(param1: string, param2: string) {
      //~^ ERROR: 'param1' is declared but its value is never read.
        param2 = param2 + "dummy value";
    }
}
