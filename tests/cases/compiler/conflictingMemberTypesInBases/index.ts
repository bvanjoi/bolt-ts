// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/conflictingMemberTypesInBases.ts`, Apache-2.0 License

interface A {
    m: string;
}
interface B extends A {
}
interface C {
    m: number;
}
interface D extends C {
}

interface E extends B { } // Error here for extending B and D
//~^ ERROR: Interface 'E' cannot simultaneously extend types 'B' and 'D'
interface E extends D { } // No duplicate error here
