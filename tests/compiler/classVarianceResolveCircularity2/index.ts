// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classVarianceResolveCircularity2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

export {};

class Bar<T> {
    num!: number;
    Value = callme(new Foo(this)).bar.num;
    // Field: number = callme(new Foo(this)).bar.num;
}
declare function callme(x: Foo<any>): Foo<any>;
declare function callme(x: object): string;

class Foo<T> {
    bar!: Bar<T>;
    constructor(bar: Bar<T>) {
        this.bar = bar;
    }
}