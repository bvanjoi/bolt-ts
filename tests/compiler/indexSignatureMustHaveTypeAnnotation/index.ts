// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexSignatureMustHaveTypeAnnotation.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I {
    // Used to be indexer, now it is a computed property
    [x]: string;
    //~^ ERROR: Cannot find name 'x'.
    [x: string];
    //~^ ERROR: An index signature must have a type annotation.
}

class C {
    // Used to be indexer, now it is a computed property
    [x]: string
    //~^ ERROR: Property 'computed' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: Cannot find name 'x'.
    
}

class C2 {
    [x: string]
    //~^ ERROR: An index signature must have a type annotation.
}