// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superCallInNonStaticMethod.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Doing {
    public instanceMethod() {
    }
}

class Other extends Doing {
    // in instance method
    public instanceMethod() {
        super.instanceMethod();
    }

    // in a lambda inside a instance method
    public lambdaInsideAnInstanceMethod() {
        () => {
            super.instanceMethod();
        }
    }

    // in an object literal inside a instance method
    public objectLiteralInsideAnInstanceMethod() {
        return {
            a: () => {
                super.instanceMethod();
            },
            b: super.instanceMethod()
        };
    }

    // in a getter
    public get accessor() {
        super.instanceMethod();

        return 0;
    }

    // in a setter
    public set accessor(value: number) {
        super.instanceMethod();
    }
    
    constructor() {
        super();
        super.instanceMethod();
    }
    
    public propertyInitializer = super.instanceMethod();
    
    public functionProperty = () => {super.instanceMethod(); };
}