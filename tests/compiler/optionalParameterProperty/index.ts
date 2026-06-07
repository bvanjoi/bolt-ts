// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalParameterProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

class C {
    p: number = 0;
}

class D extends C { 
  //~^ ERROR: Class 'D' incorrectly extends base class 'C'.
    constructor(public p?: number) {
        super();
    }
}