// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/uniqueSymbolAssignmentOnGlobalAugmentationSuceeds.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict

const FOO_SYMBOL = Symbol('Foo');

declare global {
    interface Promise<T> {
        [FOO_SYMBOL]?: number;
    }
}

export function foo<T>(p: Promise<T>) {
    p[FOO_SYMBOL] = 3;
}