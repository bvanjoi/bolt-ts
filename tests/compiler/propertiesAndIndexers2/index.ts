// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertiesAndIndexers2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface A {
    [n: number]: string;
    //~^ ERROR: 'number' index type 'string' is not assignable to 'string' index type 'number'.
    [s: string]: number;
}

// All of these should fail.
interface B extends A {
    c: string;
    //~^ ERROR: Property 'c' of type 'string' is not assignable to 'string' index type 'number'.
    3: string;
    //~^ ERROR: Property '3' of type 'string' is not assignable to 'string' index type 'number'.
    Infinity: string;
    //~^ ERROR: Property 'Infinity' of type 'string' is not assignable to 'string' index type 'number'.
    "-Infinity": string;
    //~^ ERROR: Property '"-Infinity"' of type 'string' is not assignable to 'string' index type 'number'.
    NaN: string;
    //~^ ERROR: Property 'NaN' of type 'string' is not assignable to 'string' index type 'number'.
    "-NaN": string;
    //~^ ERROR: Property '"-NaN"' of type 'string' is not assignable to 'string' index type 'number'.
    6(): string;
    //~^ ERROR: Property '6' of type '() => string' is not assignable to 'number' index type 'string'.
    //~| ERROR: Property '6' of type '() => string' is not assignable to 'string' index type 'number'.
}