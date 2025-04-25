// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericGetter3.ts`, Apache-2.0 License

class A<T> { }

class C<T> {
    data: A<T>;
    get x(): A<T> {
        return this.data;
    }
}

var c = new C<number>();
var r: string = c.x;
//~^ ERROR: Type 'A<number>' is not assignable to type 'string'.