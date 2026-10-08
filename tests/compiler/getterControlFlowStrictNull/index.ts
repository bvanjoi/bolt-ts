// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/getterControlFlowStrictNull.ts`, Apache-2.0 License

//@compiler-options: strictNullChecks
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class A {
   a(): string | null {
    //~^ ERROR: Function lacks ending return statement and return type does not include 'undefined'.
        if (Math.random() > 0.5) {
            return '';
        }

        // it does error here as expected
    }
}
class B {
    get a(): string | null {
    //~^ ERROR: Function lacks ending return statement and return type does not include 'undefined'.
        if (Math.random() > 0.5) {
            return '';
        }

        // it should error here because it returns undefined
    }
}
class C {
    get a() {
    //~^ ERROR: Function lacks ending return statement and return type does not include 'undefined'.
        if (Math.random() > 0.5) {
            return 0;
        }

        // it should error here because it returns undefined
    }

    set a(value: number) {
    }
}