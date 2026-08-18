// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/getAndSetNotIdenticalType3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A<T> { foo: T; }
//~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.

class C<T> {
    data: A<number>;
//~^ ERROR: Property 'data' has no initializer and is not definitely assigned in the constructor.
    get x(): A<number> {
        return this.data;
    }
    set x(v: A<string>) {
        this.data = v;
        //~^ ERROR: Type 'A<string>' is not assignable to type 'A<number>'.
    }
}

var x = new C();
var r = x.x;
x.x = r;
//~^ ERROR: Type 'A<number>' is not assignable to type 'A<string>'.