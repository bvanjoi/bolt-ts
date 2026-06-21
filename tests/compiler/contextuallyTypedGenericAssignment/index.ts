// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypedGenericAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo<A extends any[]>(
    arg: <T extends { a: number }>(t: T, ...rest: A) => number
) { }

foo((t, u: number) => t.a)
