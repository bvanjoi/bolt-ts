// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/coAndContraVariantInferences5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Thing = 'a' | 'b';

function f(
    options: SelectOptions<Thing>,
    onChange: (status: Thing | null) => void,
): void {
    select({
        options,
        onChange,
    });
}

declare function select<KeyT extends string>(props: SelectProps<KeyT>): void;

type SelectProps<KeyT extends string> = {
    options?: SelectOptions<KeyT>;
    onChange: (key: KeyT) => void;
};

type SelectOptions<KeyT extends string> =
    | Array<{key: KeyT}>
    | Array<KeyT>;
