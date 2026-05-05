// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonNullMappedType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

function f<A extends string>(p0: { [key in A]: {} | undefined }, p1: A) {
    const v: {} = p0[p1]!;
}