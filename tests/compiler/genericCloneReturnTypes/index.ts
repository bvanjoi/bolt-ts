// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCloneReturnTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Bar<T> {

    public size: number;
    public t: T;
    //~^ ERROR: Property 't' has no initializer and is not definitely assigned in the constructor.

    constructor(x: number) {

        this.size = x;

    }

    public clone() {

        return new Bar<T>(this.size);

    }

}

var b: Bar<number>;

var b2 = b.clone();
//~^ ERROR: Variable 'b' is used before being assigned.
//~| ERROR: Variable 'b' is used before being assigned.
var b3: Bar<string>;
b = b2;
b = b3;
//~^ ERROR: Variable 'b3' is used before being assigned.
//~| ERROR: Type 'Bar<string>' is not assignable to type 'Bar<number>'.
