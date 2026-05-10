// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/conditionalEqualityTestingNullability.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

export type Equals<A1 extends any, A2 extends any> =
    (<A>() => A extends A1 ? 1 : 0) extends (<A>() => A extends A2 ? 1 : 0)
    ? 1
    : 0

interface Foo<T> {
    x : () => T
}

declare const a: Foo<Date>;
declare const b: Foo<Date | null>;

//Expected 0, Actual 1
type ShouldBe0 = Equals<typeof a, typeof b>;