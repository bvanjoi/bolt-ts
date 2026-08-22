// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertiesAndIndexers.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface X { }
interface Y {
    n: number;
}
interface Z {
    s: string;
}

interface A {
    a: Y;
    b: X;
    1: Z;
}

interface B extends A {
    [n: number]: string;
    //~^ ERROR: Property '1' of type 'Z' is not assignable to 'number' index type 'string'.
    c: boolean;
    3: boolean;
    //~^ ERROR: Property '3' of type 'boolean' is not assignable to 'number' index type 'string'.
    6(): string;
    //~^ ERROR: Property '6' of type '() => string' is not assignable to 'number' index type 'string'.
}

interface B {
    4: boolean;
    //~^ ERROR: Property '4' of type 'boolean' is not assignable to 'number' index type 'string'.
    5: string;
}

interface C extends A {
    [s: string]: number;
    //~^ ERROR: Property '1' of type 'Z' is not assignable to 'string' index type 'number'.
    //~| ERROR: Property 'a' of type 'Y' is not assignable to 'string' index type 'number'.
    //~| ERROR: Property 'b' of type 'X' is not assignable to 'string' index type 'number'.
    c: boolean;
    //~^ ERROR: Property 'c' of type 'boolean' is not assignable to 'string' index type 'number'.
    3: boolean;
    //~^ ERROR: Property '3' of type 'boolean' is not assignable to 'string' index type 'number'.
}

interface D extends B, C {
  //~^ ERROR: Property '4' of type 'boolean' is not assignable to 'string' index type 'number'.
  //~| ERROR: Property '5' of type 'string' is not assignable to 'string' index type 'number'.
  //~| ERROR: Property '6' of type '() => string' is not assignable to 'string' index type 'number'.
  //~| ERROR: 'number' index type 'string' is not assignable to 'string' index type 'number'.
    2: Z;
    //~^ ERROR: Property '2' of type 'Z' is not assignable to 'number' index type 'string'.
    //~| ERROR: Property '2' of type 'Z' is not assignable to 'string' index type 'number'.
    Infinity: number;
    //~^ ERROR: Property 'Infinity' of type 'number' is not assignable to 'number' index type 'string'.
    zoo: string;
    //~^ ERROR: Property 'zoo' of type 'string' is not assignable to 'string' index type 'number'.
}

class P {
    [n: string]: string
}

class Q extends P {
    t: number;
    //~^ ERROR: Property 't' of type 'number' is not assignable to 'string' index type 'string'.
    //~| ERROR: Property 't' has no initializer and is not definitely assigned in the constructor.
}

var c: {
    [n: number]: string;
    c: boolean;
    3: boolean;
    //~^ ERROR: Property '3' of type 'boolean' is not assignable to 'number' index type 'string'.
};