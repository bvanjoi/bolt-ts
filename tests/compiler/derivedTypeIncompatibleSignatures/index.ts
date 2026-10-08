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
    //~^ ERROR: 'number' index signatures are incompatible.
    [a: string]: number; // Number is not a subtype of string.  Should error.
}

interface G {
    [a: number]: string;
}

interface H extends G {
    //~^ ERROR: 'number' index signatures are incompatible.
    [a: number]: number; // Should error for the same reason
}