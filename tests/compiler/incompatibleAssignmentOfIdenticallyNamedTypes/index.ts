// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/incompatibleAssignmentOfIdenticallyNamedTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface T { }
declare const a: T;
class Foo<T> {
    x: T;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
    fn() {
        this.x = a;
        //~^ ERROR: Type 'T' is not assignable to type 'T'.
    }
}
