// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superCallFromClassThatDerivesNonGenericTypeButWithTypeArguments1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class A {
    constructor(private map: (value: number) => string) {

    }
}

class B extends A<number, string> {
            //~^ ERROR: Type 'A' is not generic.
    constructor() { super(value => String(value)); }
}