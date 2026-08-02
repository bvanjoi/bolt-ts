// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/subtypingTransitivity.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class B {
    x: Object;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}

class D extends B {
    public x: string;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}
class D2 extends B {
    public x: number;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}

var b: B;
var d: D;
var d2: D2;

d.x = '';
//~^ ERROR: Variable 'd' is used before being assigned.
b = d;
//~^ ERROR: Variable 'd' is used before being assigned.
b.x = 1; // assigned number to string