class C123 {
    [s: string]: number;
    constructor() {
    }
}

interface D123 extends C123 {
    x: number;
    y: string;
    //~^ ERROR: Property 'y' of type 'string' is not assignable to 'number' index type 'number'.
}