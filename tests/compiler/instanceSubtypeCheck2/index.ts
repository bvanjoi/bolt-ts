// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/instanceSubtypeCheck2.ts`, Apache-2.0 License

class C1<T> {
    x: C2<T>;
}

class C2<T> extends C1<T> {
    x: string
    //~^ ERROR: Type 'string' is not assignable to type 'C2<T>'.
}