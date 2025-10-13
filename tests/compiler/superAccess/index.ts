// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superAccess.ts`, Apache-2.0 License

class MyBase {
    static S1: number = 5;
    private S2: string = "test";
    f = () => 5;
}

class MyDerived extends MyBase {
    foo() {
        var l3 = super.S1;    // Expected => Error: Only public instance methods of the base class are accessible via the 'super' keyword
        //~^ ERROR: Property 'S1' does not exist on type 'MyBase<MyDerived>'.
        var l4 = super.S2;    // Expected => Error: Only public instance methods of the base class are accessible via the 'super' keyword
        //~^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
        var l5 = super.f();   // Expected => Error: Only public instance methods of the base class are accessible via the 'super' keyword
        //~^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
    }
}