// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/conditionalTypeSubclassExtendsTypeParam.ts`, Apache-2.0 License

declare class Model<M extends MR, MR extends {}> {
    public getField2<K extends keyof M>(): Field<M[K], [K] extends [keyof MR] ? MR[K] : M[K]>
}

declare class Field<T extends TR, TR> {
}