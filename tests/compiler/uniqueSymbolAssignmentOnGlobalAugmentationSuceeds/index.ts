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

type Tag<Token extends string> = Token;
type GetTagMetadata<Type extends Tag<TagName>, TagName extends string> = string;
const _a = '' as GetTagMetadata<'URL', 'NonExistentTag'>;
//~^ ERROR: Type '"URL"' does not satisfy the constraint '"NonExistentTag"'.