class C123 {
    [s: string]: number;
    constructor() {
    }
}

class D123 extends C123 {
    x: number;
    y: string;
    //~^ ERROR: Property 'y' of type 'string' is not assignable to 'string' index type 'number'.
}