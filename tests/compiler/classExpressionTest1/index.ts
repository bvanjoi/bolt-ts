// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/classExpressionTest1.ts`, Apache-2.0 License

function M() {
    class C<X> {
        f<T>() {
            var t: T;
            var x: X;
            return { t, x };
        }
    }

    var v = new C<number>();
    return v.f<string>();
}