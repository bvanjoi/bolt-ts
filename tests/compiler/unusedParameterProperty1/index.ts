// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedParameterProperty1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class A {
    constructor(private used: string) {
    //~^ ERROR: Property 'used' is declared but its value is never read.
        let foge = used;
    //~^ ERROR: 'foge' is declared but its value is never read.
    }
}