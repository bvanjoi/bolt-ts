// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/homomorphicMappedTypeIntersectionAssignability.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

function f<TType>(
    a: { weak?: string } & Readonly<TType> & { name: "ok" },
    b: Readonly<TType & { name: string }>,
    c: Readonly<TType> & { name: string }) {
    c = a; // Works
    b = a; // Should also work
}