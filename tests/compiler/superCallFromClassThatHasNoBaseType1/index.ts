// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superCallFromClassThatHasNoBaseType1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class A {
    constructor(private map: (value: number) => string) {

    }
}

class B {
    constructor() { super(value => String(value)); }
    //~^ ERROR: 'super' can only be referenced in a derived class.
}