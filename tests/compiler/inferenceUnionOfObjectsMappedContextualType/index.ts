// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceUnionOfObjectsMappedContextualType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type Entity = {
    someDate: Date | null;
} & ({ id: string; } | { id: number; })

type RowRendererMeta<TInput extends {}> = {
    [Key in keyof TInput]: { key: Key; caption: string; formatter?: (value: TInput[Key]) => string; };
}

type RowRenderer<TInput extends {}> = RowRendererMeta<TInput>[keyof RowRendererMeta<TInput>];

const test: RowRenderer<Entity> = {
    key: 'someDate',
    caption: 'My Date',
    formatter: (value) => value ? value.toString() : '-' // value: any
}

const test2: RowRenderer<Entity> = {
    key: 'someDate',
    caption: 'My Date',
    formatter: (value) => value.toString()
    //~^ ERROR: 'value' is possibly 'null'.
}
