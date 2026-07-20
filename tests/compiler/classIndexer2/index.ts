class C123 {
    [s: string]: number;
    x: number;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
    y: string;
    //~^ ERROR: Property 'y' of type 'string' is not assignable to 'string' index type 'number'.
    //~| ERROR: Property 'y' has no initializer and is not definitely assigned in the constructor.
    constructor() {
    }
}
