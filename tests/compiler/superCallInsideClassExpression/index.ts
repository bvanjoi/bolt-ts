// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superCallInNonStaticMethod.ts`, Apache-2.0 License

//@compiler-options: target=es2015
class A {
}

class C {
}

class B extends A {
    constructor() {
    //~^ ERROR: Constructors for derived classes must contain a 'super' call.

        var D = class extends C {
            constructor() {
                super();
            }
        }
    }
}