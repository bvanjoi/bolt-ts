// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/derivedClassConstructorWithExplicitReturns01.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//compiler-options: sourcemap

class C {
    cProp = 10;

    foo() { return "this never gets used."; }

    constructor(value: number) {
        return {
            cProp: value,
            foo() {
                return "well this looks kinda C-ish.";
            }
        }
    }
}

class D extends C {
    dProp = () => this;

    constructor(a = 100) {
        super(a);

        if (Math.random() < 0.5) {
            "You win!"
            return {
                cProp: 1,
                dProp: () => this,
                foo() { return "You win!!!!!" }
            };
        }
        else
            return null;
          //~^ ERROR: Type 'null' is not assignable to type 'D'.
          //~| ERROR: Return type of constructor signature must be assignable to the instance type of the class.
    }
}