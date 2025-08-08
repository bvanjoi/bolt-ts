// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/classImplementsClass7.ts`, Apache-2.0 License

class A {
    private x: number;
}

class B implements A {}
//~^ ERROR: Property 'x' is missing.
