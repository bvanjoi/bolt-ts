// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mutuallyRecursiveInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class T<A> {
    a: A;
    //~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
    b: any
}
class L<RT extends { a: 'a' | 'b', b: any }> extends T<RT[RT['a']]> {
    m() { this.a }
}
class X  {
    a: 'a' | 'b'
    //~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
    b: number
    //~^ ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor.
    m2() {
        this.a
    }
}
