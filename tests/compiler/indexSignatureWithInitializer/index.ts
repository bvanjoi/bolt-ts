// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/indexSignatureWithInitializer.ts`, Apache-2.0 License

// These used to be indexers, now they are computed properties
interface I {
    [x = '']: string;
    //~^ ERROR: A computed property name in an interface must refer to an expression whose type is a literal type or a 'unique symbol' type.
    //~| ERROR: Cannot find name 'x'.
}

class C {
    [x = 0]: string
    //~^ ERROR: A computed property name in a class property declaration must have a simple literal type or a 'unique symbol' type.
    //~| ERROR: Cannot find name 'x'.
}