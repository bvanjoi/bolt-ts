// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/instanceSubtypeCheck2.ts`, Apache-2.0 License

class C1<T> {
    x: C2<T>;
}

class C2<T> extends C1<T> {
    x: string
    //~^ ERROR: Property 'x' in type 'C2<T, C2>' is not assignable to the same property in base type 'C1<T, C2>'
}