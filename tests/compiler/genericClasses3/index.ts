// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericClasses3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

class B<T> {
    a: T;
    //~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
    b: T;
    //~^ ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor.
}

class C<T> extends B<T> {
    public x: T;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}

var v2: C <string>;

var y = v2.x; // should be 'string'
//~^ ERROR: Variable 'v2' is used before being assigned.
var u = v2.a; // should be 'string'
//~^ ERROR: Variable 'v2' is used before being assigned.

var z = v2.b;
//~^ ERROR: Variable 'v2' is used before being assigned.

