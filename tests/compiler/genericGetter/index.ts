// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericGetter.ts`, Apache-2.0 License

class C<T> {
    data: T;
    get x(): T {
        return this.data;
    }
}

var c = new C<number>();
var r: string = c.x;
//~^ ERROR: Type 'number' is not assignable to type 'string'.