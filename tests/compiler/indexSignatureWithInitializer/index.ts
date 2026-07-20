// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexSignatureWithInitializer.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// These used to be indexers, now they are computed properties
interface I {
    [x = '']: string;
    //~^ ERROR: A computed property name in an interface must refer to an expression whose type is a literal type or a 'unique symbol' type.
    //~| ERROR: Cannot find name 'x'.
}

class C {
    [x = 0]: string
    //~^ ERROR: A computed property name in a class property declaration must have a simple literal type or a 'unique symbol' type.
    //~| ERROR: Property 'computed' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: Cannot find name 'x'.
}
