// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace a {
    export function foo(x: number) {
        return x;
    }
}

namespace c {
    import b = a.foo;
    export var bVal = b(10);
    export var bVal2 = b;
}
