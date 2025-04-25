// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericGetter2.ts`, Apache-2.0 License

class A<T> { }

class C<T> {
    data: A<T>;
    get x(): A { //~ ERROR: Generic type 'A<T>' requires 1 type argument.
        return this.data;
    }
}