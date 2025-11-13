// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritSameNamePropertiesWithDifferentOptionality.ts`, Apache-2.0 License

interface C {
    x?: number;
}

interface C2 {
    x: number;
}

interface A extends C, C2 { // error
    //~^ ERROR: Interface 'A' cannot simultaneously extend types 'C' and 'C2'.
    y: string;
}

interface A1 extends C {
    y: string;
}

interface A2 extends C2 {
    y: string;
}