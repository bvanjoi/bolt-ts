// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritedConstructorWithRestParams.ts`, Apache-2.0 License

class Base {
    constructor(...a: string[]) { }
}

class Derived extends Base { }

// Ok
new Derived("", "");
new Derived("");
new Derived();

// Errors
new Derived("", 3);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
new Derived(3);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.