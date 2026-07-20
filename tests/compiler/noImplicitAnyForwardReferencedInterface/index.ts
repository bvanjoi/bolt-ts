// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyForwardReferencedInterface.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

declare var x: Entry;

interface Entry {
    // Should return error for implicit any.
    new ();
    //~^ ERROR: Construct signature, which lacks return-type annotation, implicitly has an 'any' return type.
}