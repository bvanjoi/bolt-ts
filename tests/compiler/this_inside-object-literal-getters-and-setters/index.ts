// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/this_inside-object-literal-getters-and-setters.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace ObjectLiteral {
    var ThisInObjectLiteral = {
        _foo: '1',
        get foo(): string {
            return this._foo;
        },
        set foo(value: string) {
            this._foo = value;
        },
        test: function () {
            return this._foo;
        }
    }
}

