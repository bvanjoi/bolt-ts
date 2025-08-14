class C123 {
    [s: string]: number;
    x: number;
    y: string;
    //~^ ERROR: Property 'y' of type 'string' is not assignable to 'string' index type 'number'.
    constructor() {
    }
}