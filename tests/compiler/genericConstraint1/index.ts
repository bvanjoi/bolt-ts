// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericConstraint1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C<T> {
    public bar2<U extends T>(x: T, y: U): T {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'T'.
    }
}

var x = new C<number>();
x.bar2<string>(2, "");
//~^ ERROR: Type 'string' does not satisfy the constraint 'number'.