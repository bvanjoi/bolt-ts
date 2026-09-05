// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constructorReturningAPrimitive.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// technically not allowed by JavaScript but we don't have a 'not-primitive' constraint
// functionally only possible when your class is otherwise devoid of members so of little consequence in practice

class A {
    constructor() {
        return 1;
    }
}

var a = new A();

class B<T> {
    constructor() {
        var x: T;
        return x;
        //~^ ERROR: Variable 'x' is used before being assigned.
        //~| ERROR: Return type of constructor signature must be assignable to the instance type of the class.
        //~| ERROR: Type 'T' is not assignable to type 'B<T>'.
    }
}

var b = new B<number>();