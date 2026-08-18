// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeIdentityConsidersBrands.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class X{
      name: string;
      //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
}

class Y{
      name: string;
      //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
}

class X_1 {
    private name: string;
    //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
}

class Y_1 {
    private name: string;
    //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
}

function foo(arg: X){}
 
var a = new Y();
var b = new X();
 
a = b; // ok
foo(a); // ok

var a2 = new Y_1();
var b2 = new X_1();

function foo2(arg: X_1) { }

a2 = b2; // should error
//~^ ERROR: Type 'X_1' is not assignable to type 'Y_1'.
foo2(a2); // should error
//~^ ERROR: Argument of type 'Y_1' is not assignable to parameter of type 'X_1'.
