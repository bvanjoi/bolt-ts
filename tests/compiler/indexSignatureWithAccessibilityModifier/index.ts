// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/indexSignatureWithAccessibilityModifier.ts`, Apache-2.0 License

interface I {
    [public x: string]: string;
    //~^ ERROR: An index signature parameter cannot have an accessibility modifier.
}

class C {
    [public x: string]: string
    //~^ ERROR: An index signature parameter cannot have an accessibility modifier.
}