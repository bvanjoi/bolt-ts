// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/classImplementsClass7.ts`, Apache-2.0 License

class A {
    private x: number;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}

class B implements A {}
//~^ ERROR: Property 'x' is missing.
