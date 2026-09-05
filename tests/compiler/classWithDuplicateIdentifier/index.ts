// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classWithDuplicateIdentifier.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    a(): number { return 0; } // error: duplicate identifier
    a: number;
    //~^ ERROR: Duplicate identifier 'a'.
    //~| ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: Subsequent property declarations must have the same type. Property 'a' must be of type '() => number', but here has type 'number'.
}
class K {
    b: number; // error: duplicate identifier
    //~^ ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor.
    b(): number { return 0; }
    //~^ ERROR: Duplicate identifier 'b'.
    //~| ERROR: Duplicate identifier 'b'.
}
class D {
    c: number;
    //~^ ERROR: Property 'c' has no initializer and is not definitely assigned in the constructor.
    c: string;
    //~^ ERROR: Property 'c' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: Duplicate identifier 'c'.
    //~| ERROR: Subsequent property declarations must have the same type. Property 'c' must be of type 'number', but here has type 'string'.
}
