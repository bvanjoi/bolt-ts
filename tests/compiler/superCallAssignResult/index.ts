// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superCallAssignResult.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class E {
    constructor(arg: any) { }
}

class H extends E {
    constructor() {
        var x = super(5); // Should be of type void, not E.
        x = 5;
        //~^ ERROR: Type 'number' is not assignable to type 'void'.
    }
}
