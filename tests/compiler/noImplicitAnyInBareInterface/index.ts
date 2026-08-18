// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyInBareInterface.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

interface Entry {
    // Should return error for implicit any on `new` and `foo`.
    new ();
    //~^ ERROR: Construct signature, which lacks return-type annotation, implicitly has an 'any' return type.
    few() : any;
    foo();
    //~^ ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.
}
