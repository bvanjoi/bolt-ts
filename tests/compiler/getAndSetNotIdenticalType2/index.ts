// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/getAndSetNotIdenticalType2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A<T> { foo: T; }
//~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.

class C<T> {
    data: A<T>;
//~^ ERROR: Property 'data' has no initializer and is not definitely assigned in the constructor.
    get x(): A<T> {
        return this.data;
    }
    set x(v: A<string>) {
        this.data = v;
        //~^ ERROR: Type 'A<string>' is not assignable to type 'A<T>'.
    }
}

var x = new C();
var r = x.x;
x.x = r;
//~^ ERROR: Type 'A<unknown>' is not assignable to type 'A<string>'.