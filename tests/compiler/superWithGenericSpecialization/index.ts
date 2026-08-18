// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superWithGenericSpecialization.ts`, Apache-2.0 License

//@compiler-options: target=es2015
class C<T> {
    x: T;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}

class D<T> extends C<string> {
    y: T;
    //~^ ERROR: Property 'y' has no initializer and is not definitely assigned in the constructor.
    constructor() {
        super(); // uses the type parameter type of the base class, ie string
    }
}

var d: D<number>;
var r: string = d.x;
//~^ ERROR: Variable 'd' is used before being assigned.
var r2: number = d.y;
//~^ ERROR: Variable 'd' is used before being assigned.
