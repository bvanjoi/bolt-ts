// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/classExpressionTest2.ts`, Apache-2.0 License

function M() {
    var m = class C<X> {
        f<T>() {
            var t: T;
            var x: X;
            return { t, x };
            //~^ ERROR: Variable 't' is used before being assigned.
            //~| ERROR: Variable 'x' is used before being assigned.
        }
    }

    var v = new m<number>();
    return v.f<string>();
}

var g = class G<T> {}
var h = new g<number>();
