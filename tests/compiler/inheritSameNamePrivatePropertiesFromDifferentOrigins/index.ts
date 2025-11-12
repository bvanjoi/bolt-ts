// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritSameNamePrivatePropertiesFromDifferentOrigins.ts`, Apache-2.0 License

class C {
    private x: number;
}

class C2 {
    private x: number;
}

interface A extends C, C2 { // error
//~^ ERROR: Interface 'A' cannot simultaneously extend types 'C' and 'C2'.
    y: string;
}