// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/gettersAndSettersErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    public get Foo() { return "foo";} // ok
    public set Foo(foo:string) {} // ok

    public Foo = 0; // error - duplicate identifier Foo - confirmed
    //~^ ERROR: Duplicate identifier 'Foo'.
    //~| ERROR: Subsequent property declarations must have the same type. Property 'Foo' must be of type 'string', but here has type 'number'.
    public get Goo(v:string):string {return null;} // error - getters must not have a parameter
    //~^ ERROR: A 'get' accessor cannot have parameters.
    //~| ERROR: Type 'null' is not assignable to type 'string'.
    public set Goo(v:string):string {} // error - setters must not specify a return type
}

class E {
    private get Baz():number { return 0; }
    //~^ ERROR: A get accessor must be at least as accessible as the setter.
    public set Baz(n:number) {} // error - accessors do not agree in visibility
}


