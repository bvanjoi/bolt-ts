// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constraintsThatReferenceOtherContstraints1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Object { }

class Foo<T, U extends T> { }
class Bar<T extends Object, U extends T> {
    data: Foo<Object, Object>; // Error 1 Type 'Object' does not satisfy the constraint 'T' for type parameter 'U extends T'.
    //~^ ERROR: Property 'data' has no initializer and is not definitely assigned in the constructor.
}

var x: Foo< { a: string }, { a: string; b: number }>; // Error 2 Type '{ a: string; b: number; }' does not satisfy the constraint 'T' for type 
