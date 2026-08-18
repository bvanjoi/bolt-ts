// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reorderProperties.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A<T> {
    x: T
}

interface B<T> {
    x: T
}

interface C<S> extends A<D<S>> {
    y: S
}

interface D<S> extends B<C<S>> {
    y: S
}

var c: C<{ s: string; n: number }>
var d: D<{ n: number; s: string }> = c
//~^ ERROR: Variable 'c' is used before being assigned.