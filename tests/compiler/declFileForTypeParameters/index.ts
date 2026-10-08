// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileForTypeParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

class C<T> {
    x: T;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
    foo(a: T): T {
        return this.x;
    }
}
