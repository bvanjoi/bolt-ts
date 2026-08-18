// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/anonymousClassExpression2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

while (0) {
    class A {
        methodA() {
            this; //note: a this reference of some kind is required to trigger the bug
        }
    }

    class B {
        methodB() {
            this.methodA; // error
            //~^ ERROR: Property 'methodA' does not exist on type 'B<B>'.
            this.methodB; // ok
        }
    }
}