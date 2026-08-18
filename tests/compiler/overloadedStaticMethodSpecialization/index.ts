// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadedStaticMethodSpecialization.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A<T> {
    static B<S>(v: A<S>): A<S>;
    static B<S>(v: S): A<S>;
    static B<S>(v: any): A<S> {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'A<S>'.
    }
}
