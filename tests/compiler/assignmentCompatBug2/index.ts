// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatBug2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var b2: { b: number;} = { a: 0 }; // error
//~^ ERROR: Object literal may only specify known properties, and 'a' does not exist in type '{ b: number; }'.
b2 = { a: 0 }; // error
//~^ ERROR: Object literal may only specify known properties, and 'a' does not exist in type '{ b: number; }'.
b2 = {b: 0, a: 0 };
//~^ ERROR: Object literal may only specify known properties, and 'a' does not exist in type '{ b: number; }'.
var b3: { f(n: number): number; g(s: string): number; m: number; n?: number; k?(a: any): any; };

b3 = {
    f: (n) => { return 0; },
    g: (s) => { return 0; },
    m: 0,
}; // ok

b3 = {  //~ERROR: Property 'm' is missing.
    f: (n) => { return 0; },
    g: (s) => { return 0; },
}; // error

b3 = {  //~ERROR: Property 'g' is missing.
    f: (n) => { return 0; },
    m: 0,
}; // error

b3 = {
    f: (n) => { return 0; },
    g: (s) => { return 0; },
    m: 0,
    n: 0,
    k: (a) =>{ return null; },
}; // ok

b3 = {  //~ERROR: Property 'm' is missing.
    f: (n) => { return 0; },
    g: (s) => { return 0; },
    n: 0,
    k: (a) =>{ return null; },
}; // error