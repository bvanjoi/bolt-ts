// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexWithoutParamType2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    // Used to be indexer, now it is a computed property
    [x]: string
    //~^ ERROR: Cannot find name 'x'.
    //~| ERROR: Property 'computed' has no initializer and is not definitely assigned in the constructor.
}
