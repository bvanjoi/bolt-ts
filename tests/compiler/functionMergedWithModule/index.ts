// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionMergedWithModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo(title: string) {
    var x = 10;
}

namespace foo.Bar {
    export function f() {
    }
}

namespace foo.Baz {
    export function g() {
        Bar.f();
    }
}
