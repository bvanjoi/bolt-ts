// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuredMaappedTypeIsNotImplicitlyAny.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

function foo<T extends string>(key: T, obj: { [_ in T]: number }) {
    const { [key]: bar } = obj; // Element implicitly has an 'any' type because type '{ [_ in T]: number; }' has no index signature.
    bar; // bar : any

    // Note: this does work:
    const lorem = obj[key];
}