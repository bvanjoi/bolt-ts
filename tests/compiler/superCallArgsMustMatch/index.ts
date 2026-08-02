// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superCallArgsMustMatch.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class T5<T>{

    public foo: T;
    //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.

    constructor(public bar: T) { }

}

 

class T6 extends T5<number>{

    constructor() {

        // Should error; base constructor has type T for first arg,
        // which is instantiated with 'number' in the extends clause
        super("hi");
        //~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.

        var x: number = this.foo;

    }

}