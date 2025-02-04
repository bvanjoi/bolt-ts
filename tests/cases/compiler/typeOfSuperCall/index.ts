// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/typeOfSuperCall.ts`, Apache-2.0 License

class C {
}

class D extends C {
    constructor() {
        var x: void = super();
    }
}