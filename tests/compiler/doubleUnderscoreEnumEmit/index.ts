// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doubleUnderscoreEnumEmit.ts`, Apache-2.0 License

//@compiler-options: target=es2015

enum Foo {
    "__a" = 1,
    "(Anonymous function)" = 2,
    "(Anonymous class)" = 4,
    "__call" = 10
}
namespace Foo {
    export function ___call(): number {
        return 5;
    }
}
function Bar() {
    return "no";
}
namespace Bar {
    export function __call(x: number): number {
        return 5;
    }
}