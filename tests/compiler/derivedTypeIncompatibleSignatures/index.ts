// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/derivedTypeIncompatibleSignatures.ts`, Apache-2.0 License

interface A {
    (a: string): string;
}

interface B extends A {
    (a: string): number; // Number is not a subtype of string.  Should error.
}

interface C {
    new (a: string): string;
}

interface D extends C {
    new (a: string): number; // Number is not a subtype of string.  Should error.
}

interface E {
    [a: string]: string;
}

interface F extends E {
    //~^ ERROR: Interface 'F' incorrectly extends interface 'E'.
    [a: string]: number; // Number is not a subtype of string.  Should error.
    //~^ ERROR: 'string' index signatures are incompatible.
}

interface G {
    [a: number]: string;
}

interface H extends G {
    //~^ ERROR: Interface 'H' incorrectly extends interface 'G'.
    [a: number]: number; // Should error for the same reason
    //~^ ERROR: 'string' index signatures are incompatible.
}